#!/usr/bin/env python3
"""
Global fit for multiple nuclear levels simultaneously.
Minimizes total chi-squared to find a SINGLE normalization factor common to all states.
"""
import numpy as np
from scipy.optimize import minimize_scalar
import glob
import os
import sys

# Configuration
CONFIG_FILE = "states.config"
RESULTS_DIR = "Results"
EXP_DATA_DIR = "experimental_data_bylevel"
OUTPUT_FILE = "Results/global_fit_results.txt"

def load_states():
    """Read state IDs and energies from config"""
    states = []
    with open(CONFIG_FILE, 'r') as f:
        for line in f:
            if line.strip() and not line.strip().startswith('#'):
                parts = line.split('|')
                states.append({
                'id': int(parts[0].strip()),
                'energy': float(parts[1].strip()), # Use Q-value from column 1
                'desc': parts[10].strip() if len(parts) > 10 else f"State {parts[0].strip()}"
                })
    return states

def load_experiment(state_id):
    """Load experimental data for a specific state"""
    # Find matching file
    pattern = f"{EXP_DATA_DIR}/level_{state_id}_*.dat"
    files = glob.glob(pattern)
    if not files:
        return None
    
    data = np.loadtxt(files[0])
    return {
        'angles': data[:, 0],
        'sigma': data[:, 1],
        'error': data[:, 2]
    }

def load_theory(state_id):
    """Load QDUC theory prediction (mean) for a specific state"""
    fname = f"{RESULTS_DIR}/output_state{state_id}.dat"
    if not os.path.exists(fname):
        return None
    
    data = np.loadtxt(fname)
    return {
        'angles': data[:, 0],
        'cross_section': data[:, 1],  # Mean cross section
        'plus1sig': data[:, 2],
        'minus1sig': data[:, 4]
    }

def fit_global():
    states = load_states()
    print(f"Loaded {len(states)} states from config.")
    
    all_exp = []
    all_theory_interp = []
    all_weights = []
    
    valid_states = []

    print("\nLoading data...")
    for state in states:
        sid = state['id']
        exp = load_experiment(sid)
        theory = load_theory(sid)
        
        if exp is None:
            print(f"  [State {sid}] Missing experimental data")
            continue
        if theory is None:
            print(f"  [State {sid}] Missing theory data (run process_multistate.py first?)")
            continue
            
        print(f"  [State {sid}] Loaded {len(exp['angles'])} points")
        
        # Interpolate theory to experimental angles
        # Note: experimental angles are a subset of theory angles usually, but interpolation is safer
        theory_at_exp = np.interp(exp['angles'], theory['angles'], theory['cross_section'])
        
        all_exp.append(exp['sigma'])
        all_theory_interp.append(theory_at_exp)
        
        # Weight = 1/sigma^2
        w = 1.0 / (exp['error']**2)
        all_weights.append(w)
        
        valid_states.append(state)

    if not valid_states:
        print("Error: No valid data found for fitting.")
        return

    # Flatten arrays for global minimization
    flat_exp = np.concatenate(all_exp)
    flat_theory = np.concatenate(all_theory_interp)
    flat_weights = np.concatenate(all_weights)
    
    # Define Chi-squared function
    def chi_squared_global(norm):
        # Chi2 = sum( weights * (y_exp - N * y_theory)^2 )
        diff = flat_exp - (norm * flat_theory)
        return np.sum(flat_weights * diff**2)
    
    # Minimize to find global N
    res = minimize_scalar(chi_squared_global, bounds=(0.1, 1000.0), method='bounded')
    best_norm = res.x
    min_chi2 = res.fun
    
    # Calculate uncertainty on N (delta-chi2 = 1)
    # Approximate parabolic error: sigma_N = sqrt(2 / d2(chi2)/dN2)
    # d(chi2)/dN = sum( 2*w*(exp - N*theo)*(-theo) )
    # d2(chi2)/dN2 = sum( 2*w*theo^2 )
    curvature = 2.0 * np.sum(flat_weights * flat_theory**2)
    norm_err = np.sqrt(2.0 / curvature) if curvature > 0 else 0.0

    total_N = len(flat_exp)
    dof = total_N - 1 # One parameter fitted
    red_chi2 = min_chi2 / dof

    print("\n" + "="*60)
    print("GLOBAL FIT RESULTS")
    print("="*60)
    print(f"Normalization Factor (N) : {best_norm:.4f} +/- {norm_err:.4f}")
    print(f"Total Chi-Squared        : {min_chi2:.4f}")
    print(f"Reduced Chi-Squared      : {red_chi2:.4f} ({min_chi2:.1f}/{dof})")
    print("="*60)
    
    # Calculate per-state statistics
    with open(OUTPUT_FILE, 'w') as f:
        f.write("# Global Fit Results\n")
        f.write(f"Normalization: {best_norm:.6f}\n")
        f.write(f"Norm_Uncertainty: {norm_err:.6f}\n")
        f.write(f"Total_Chi2: {min_chi2:.6f}\n")
        f.write(f"Reduced_Chi2: {red_chi2:.6f}\n")
        f.write("\n# Per-State Breakdown\n")
        f.write(f"{'State':<6} {'Desc':<25} {'Chi2':<10} {'RedChi2':<10} {'N_pts':<6}\n")
        f.write("-" * 65 + "\n")
        
        print(f"\nBreakdown by State (using Global N={best_norm:.2f}):")
        print(f"{'State':<6} {'Chi2':<10} {'RedChi2':<10} {'Status':<10}")
        print("-" * 40)
        
        for i, state in enumerate(valid_states):
            # Recalculate chi2 for this state using global N
            y_exp = all_exp[i]
            y_theo = all_theory_interp[i]
            w = all_weights[i]
            
            diff = y_exp - (best_norm * y_theo)
            state_chi2 = np.sum(w * diff**2)
            n_pts = len(y_exp)
            state_red_chi2 = state_chi2 / n_pts # Not strictly correct DOF def for subset, but useful metric
            
            f.write(f"{state['id']:<6d} {state['desc']:<25} {state_chi2:<10.4f} {state_red_chi2:<10.4f} {n_pts:<6d}\n")
            print(f"{state['id']:<6d} {state_chi2:<10.4f} {state_red_chi2:<10.4f} {'OK' if state_red_chi2 < 2 else 'Poor'}")

    print(f"\nResults saved to {OUTPUT_FILE}")

if __name__ == "__main__":
    fit_global()
