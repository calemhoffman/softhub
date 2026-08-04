#!/usr/bin/env python3
"""
Fit all 416 individual KDUQ calculations independently to experimental data.
Calculates mean and RMS of resulting spectroscopic factors.
"""
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize_scalar
import glob
import sys
import os

def main():
    # Parse command line arguments
    if len(sys.argv) < 2:
        print("Usage: python3 fit_individual_kduq.py <state_id>")
        print("Example: python3 fit_individual_kduq.py 1")
        sys.exit(1)

    state_id = sys.argv[1]
    results_dir = "Results"
    os.makedirs(results_dir, exist_ok=True)

    # 1. Load Experimental Data
    # Preferred: dt35S by-level files, which are numbered by *level* index
    # (level = state_id - 1, since state 1 is the 0 keV ground state = level 0).
    # Fall back to the older level_<state_id>_*.dat naming, then the global file.
    exp_data_file = 'experimental_data.dat'
    level_index = int(state_id) - 1
    dt_files = glob.glob(f'experimental_data_bylevel/dt35S_level_{level_index}_*.dat')
    legacy_files = glob.glob(f'experimental_data_bylevel/level_{state_id}_*.dat')
    if dt_files:
        exp_data_file = dt_files[0]
        print(f"Using specific experimental data: {exp_data_file}")
    elif legacy_files:
        exp_data_file = legacy_files[0]
        print(f"Using specific experimental data: {exp_data_file}")
    else:
        print(f"Using default experimental data: {exp_data_file}")
    
    if not os.path.exists(exp_data_file):
        print(f"ERROR: Experimental data file not found: {exp_data_file}")
        sys.exit(1)

    exp_data = np.loadtxt(exp_data_file)
    exp_angles = exp_data[:, 0]
    exp_cross_section = exp_data[:, 1]
    exp_errors = exp_data[:, 2]

    # 2. Get L-transfer for angular cuts
    config_file = 'states.config'
    l_transfer = 0
    with open(config_file, 'r') as f:
        for line in f:
            line = line.strip()
            if line.startswith('#') or not line:
                continue
            parts = [p.strip() for p in line.split('|')]
            if parts[0] == state_id:
                l_transfer = int(parts[2])
                break
    
    # Apply L=1 cut (angles < 30)
    if l_transfer == 1:
        mask = exp_angles < 30.0
        exp_angles_fit = exp_angles[mask]
        exp_cs_fit = exp_cross_section[mask]
        exp_err_fit = exp_errors[mask]
        print(f"L=1 cut applied: fitting {len(exp_angles_fit)} points < 30 degrees")
    else:
        exp_angles_fit = exp_angles
        exp_cs_fit = exp_cross_section
        exp_err_fit = exp_errors

    weights = 1.0 / (exp_err_fit**2)

    # 3. Load all 416 KDUQ outputs
    outputs_dir = f'sim_data/Outputs_state{state_id}'
    individual_files = sorted(glob.glob(f'{outputs_dir}/21.s{state_id}t*'))
    
    if not individual_files:
        print(f"ERROR: No KDUQ outputs found in {outputs_dir}")
        sys.exit(1)
    
    print(f"Processing {len(individual_files)} individual calculations...")

    spectroscopic_factors = []
    
    # 4. Perform Independent Fits
    for i, fname in enumerate(individual_files):
        # Load theory calculation
        theory_data = np.loadtxt(fname)
        th_angles = theory_data[:, 0]
        th_cs = theory_data[:, 1]
        
        # Interpolate theory to experimental angles
        theory_at_exp = np.interp(exp_angles_fit, th_angles, th_cs)
        
        def chi_squared_func(norm_factor):
            normalized_theory = norm_factor * theory_at_exp
            chi_sq = np.sum(weights * (exp_cs_fit - normalized_theory)**2)
            return chi_sq
        
        # Minimize Chi^2 to find Spectroscopic Factor
        res = minimize_scalar(chi_squared_func, bounds=(0.001, 1000), method='bounded')
        spectroscopic_factors.append(res.x)
        
        if (i+1) % 50 == 0:
            print(f"  Processed {i+1}/416...")

    spectroscopic_factors = np.array(spectroscopic_factors)

    # 5. Statistical Analysis
    mean_sf = np.mean(spectroscopic_factors)
    std_sf = np.std(spectroscopic_factors) 
    rms_error = np.sqrt(np.mean(spectroscopic_factors**2)) # Strictly root-mean-square?
    # Usually uncertainty is the standard deviation (RMS of residuals from mean)
    # The prompt asked for "error from the rms of the 416 factors"
    # In physics context, this often means the RMS deviation from the mean (std dev)
    # or the actual Root Mean Square. Let's provide both in output.
    
    print("\n" + "="*40)
    print(f"RESULTS FOR STATE {state_id}")
    print("="*40)
    print(f"Average Spectroscopic Factor: {mean_sf:.4f}")
    print(f"Standard Deviation:           {std_sf:.4f}")
    print(f"RMS of SF values:             {rms_error:.4f}")
    print("="*40)

    # 6. Save Results
    output_txt = f"{results_dir}/fit_individual_state{state_id}.txt"
    with open(output_txt, 'w') as f:
        f.write(f"Individual Fitting Results for State {state_id}\n")
        f.write(f"Number of calculations: {len(spectroscopic_factors)}\n")
        f.write(f"Average SF: {mean_sf:.6f}\n")
        f.write(f"Std Dev:    {std_sf:.6f}\n")
        f.write(f"RMS Value:  {rms_error:.6f}\n\n")
        f.write("All 416 Spectroscopic Factors:\n")
        for val in spectroscopic_factors:
            f.write(f"{val:.6f}\n")

    # 7. Visualization
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 12))
    
    # Histogram of SF values
    ax1.hist(spectroscopic_factors, bins=30, color='skyblue', edgecolor='black')
    ax1.axvline(mean_sf, color='red', linestyle='--', label=f'Mean = {mean_sf:.3f}')
    ax1.set_title(f'Distribution of Spectroscopic Factors (State {state_id})')
    ax1.set_xlabel('Spectroscopic Factor (C²S)')
    ax1.set_ylabel('Frequency')
    ax1.legend()

    # All 416 curves vs Data
    ax2.errorbar(exp_angles, exp_cross_section, yerr=exp_errors, fmt='ko', label='Exp Data')

    # Build a common angle grid from the first theory file and accumulate every
    # individually-normalized curve (SF_i * theory_i) so we can plot their mean.
    ref_angles = np.loadtxt(individual_files[0])[:, 0]
    normalized_curves = np.zeros((len(individual_files), len(ref_angles)))
    for i, fname in enumerate(individual_files):
        data = np.loadtxt(fname)
        norm_curve = spectroscopic_factors[i] * np.interp(ref_angles, data[:, 0], data[:, 1])
        normalized_curves[i] = norm_curve
        if i % 10 == 0:  # Plot every 10th to keep it clean
            ax2.plot(ref_angles, norm_curve, color='gray', alpha=0.1, linewidth=0.5)

    # Mean fit curve = average of the individually-normalized curves (consistent
    # with the SFs that were actually fit to the 21.s{id}t* outputs).
    mean_curve = normalized_curves.mean(axis=0)
    ax2.plot(ref_angles, mean_curve, 'r-', linewidth=2, label='Mean of Fits')

    ax2.set_yscale('log')
    ax2.set_xlim(0, 60)
    ax2.set_xlabel('Angle (deg)')
    ax2.set_ylabel('dσ/dΩ (mb/sr)')
    ax2.set_title('Individual KDUQ Fits (sample of 40/416)')
    ax2.legend()
    
    plt.tight_layout()
    output_plot = f"{results_dir}/fit_individual_state{state_id}.png"
    plt.savefig(output_plot, dpi=300)
    print(f"\nResults saved to {output_txt}")
    print(f"Plot saved to {output_plot}")

if __name__ == "__main__":
    main()
