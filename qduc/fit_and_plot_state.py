#!/usr/bin/env python3
"""
Unified script to fit and plot results for a specific state
Works with multi-state system - specify state ID as argument
"""
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize_scalar
import glob
import sys
import os

# Parse command line arguments
if len(sys.argv) < 2:
    print("Usage: python3 fit_and_plot_state.py <state_id>")
    print("Example: python3 fit_and_plot_state.py 1")
    sys.exit(1)

state_id = sys.argv[1]

# File paths
exp_data_file = 'experimental_data.dat'
theory_file = f'Results/output_state{state_id}.dat'
outputs_dir = f'Outputs_state{state_id}'
output_plot = f'Results/fit_state{state_id}.png'
output_txt = f'Results/fit_state{state_id}.txt'

# Check files exist
if not os.path.exists(theory_file):
    print(f"ERROR: Theory file not found: {theory_file}")
    print(f"Run 'python3 process_multistate.py' first!")
    sys.exit(1)

if not os.path.exists(exp_data_file):
    print(f"WARNING: Experimental data file not found: {exp_data_file}")
    print(f"Creating theory-only plot...")
    has_exp_data = False
else:
    has_exp_data = True

# Load theoretical predictions
theory_data = np.loadtxt(theory_file)
theory_angles = theory_data[:, 0]
theory_mean = theory_data[:, 1]
theory_plus1sigma = theory_data[:, 2]
theory_plus2sigma = theory_data[:, 3]
theory_minus1sigma = theory_data[:, 4]
theory_minus2sigma = theory_data[:, 5]

# Load all individual KDUQ calculations for this state
individual_files = sorted(glob.glob(f'{outputs_dir}/21.s{state_id}t*'))
print(f"\nLoading {len(individual_files)} individual KDUQ calculations for state {state_id}...")
all_individual_data = []
for fname in individual_files:
    data = np.loadtxt(fname)
    all_individual_data.append(data[:, 1])
all_individual_data = np.array(all_individual_data)

# If experimental data exists, perform fit
if has_exp_data:
    exp_data = np.loadtxt(exp_data_file)
    exp_angles = exp_data[:, 0]
    exp_cross_section = exp_data[:, 1]
    exp_errors = exp_data[:, 2]
    
    exp_error_percent = (exp_errors / exp_cross_section) * 100
    weights = 1.0 / (exp_errors**2)
    weights_normalized = weights / np.sum(weights)
    
    theory_at_exp = np.interp(exp_angles, theory_angles, theory_mean)
    
    def chi_squared_func(norm_factor):
        normalized_theory = norm_factor * theory_at_exp
        chi_sq = np.sum(weights * (exp_cross_section - normalized_theory)**2)
        return chi_sq
    
    result = minimize_scalar(chi_squared_func, bounds=(0.1, 200), method='bounded')
    optimal_norm = result.x
    min_chi_squared = result.fun
    reduced_chi_squared = min_chi_squared / (len(exp_angles) - 1)
    
    theory_normalized = optimal_norm * theory_at_exp
    residuals = exp_cross_section - theory_normalized
    normalized_residuals = residuals / exp_errors
    
    def find_norm_uncertainty():
        target_chi_sq = min_chi_squared + 1
        norm_upper = optimal_norm
        for factor in np.linspace(optimal_norm, optimal_norm * 2, 100):
            if chi_squared_func(factor) >= target_chi_sq:
                norm_upper = factor
                break
        norm_lower = optimal_norm
        for factor in np.linspace(optimal_norm, optimal_norm * 0.5, 100):
            if chi_squared_func(factor) >= target_chi_sq:
                norm_lower = factor
                break
        return (norm_upper - optimal_norm + optimal_norm - norm_lower) / 2
    
    norm_uncertainty = find_norm_uncertainty()
    
    # Print fit statistics
    print("=" * 80)
    print(f"FIT RESULTS FOR STATE {state_id}")
    print("=" * 80)
    print(f"Optimal normalization factor: {optimal_norm:.4f} ± {norm_uncertainty:.4f}")
    print(f"Chi-squared (χ²): {min_chi_squared:.4f}")
    print(f"Reduced chi-squared (χ²/ν): {reduced_chi_squared:.4f}")
    print(f"Degrees of freedom (ν): {len(exp_angles) - 1}")
    print("=" * 80)
else:
    optimal_norm = 1.0
    norm_uncertainty = 0.0

# Create visualization
c_bg = '#FFFFFF'
c_data = '#FF00FF'
c_theory = '#00CED1'
c_fill1 = '#FFD700'
c_fill2 = '#FF69B4'
c_resid = '#32CD32'
c_err = '#FF4500'
c_weight = '#9400D3'
c_grid = '#E0E0E0'
c_individual = '#CCCCCC'

plt.rcParams['font.family'] = 'monospace'

if has_exp_data:
    fig = plt.figure(figsize=(14, 14), facecolor=c_bg)
    gs = fig.add_gridspec(5, 1, height_ratios=[3, 2, 1, 1, 1], hspace=0.3)
    ax1 = fig.add_subplot(gs[0])
    ax2 = fig.add_subplot(gs[1])
    ax3 = fig.add_subplot(gs[2])
    ax4 = fig.add_subplot(gs[3])
    ax5 = fig.add_subplot(gs[4])
else:
    fig = plt.figure(figsize=(14, 8), facecolor=c_bg)
    gs = fig.add_gridspec(2, 1, height_ratios=[3, 2], hspace=0.3)
    ax1 = fig.add_subplot(gs[0])
    ax2 = fig.add_subplot(gs[1])

# Panel 1: Main plot with ±2σ bands
if has_exp_data:
    ax1.errorbar(exp_angles, exp_cross_section, yerr=exp_errors, 
                 fmt='s', color=c_data, markersize=8, capsize=5, capthick=2,
                 markeredgecolor='black', ecolor=c_data,
                 label='Experimental Data', zorder=4)

ax1.fill_between(theory_angles, 
                  optimal_norm * theory_minus2sigma, 
                  optimal_norm * theory_plus2sigma, 
                  alpha=0.2, color=c_fill2, label='Theory ±2σ', zorder=1)

ax1.fill_between(theory_angles, 
                  optimal_norm * theory_minus1sigma, 
                  optimal_norm * theory_plus1sigma, 
                  alpha=0.4, color=c_fill1, label='Theory ±1σ', zorder=2)

ax1.plot(theory_angles, optimal_norm * theory_mean, '-', color=c_theory, linewidth=3, 
         label=f'Theory × {optimal_norm:.2f}', zorder=3)

ax1.set_yscale('log')
ax1.set_ylabel('Cross Section (mb/sr)', fontsize=12, fontweight='bold', color=c_weight)
if has_exp_data:
    ax1.set_title(f'State {state_id}: Data vs Theory (Weighted Fit)\n' + 
                  f'Normalization = {optimal_norm:.3f} ± {norm_uncertainty:.3f}, χ²/ν = {reduced_chi_squared:.3f}', 
                  fontsize=14, fontweight='bold', color=c_weight)
else:
    ax1.set_title(f'State {state_id}: Theory Predictions', 
                  fontsize=14, fontweight='bold', color=c_weight)

ax1.legend(fontsize=11, loc='upper right', framealpha=1, edgecolor=c_weight)
ax1.grid(True, which='both', color=c_grid, linestyle='--', linewidth=0.5)
ax1.set_xlim(0, 60)
ax1.tick_params(labelbottom=False, colors=c_weight, which='both')
for spine in ax1.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)

# Panel 2: All individual calculations
if has_exp_data:
    ax2.errorbar(exp_angles, exp_cross_section, yerr=exp_errors, 
                 fmt='s', color=c_data, markersize=8, capsize=5, capthick=2,
                 markeredgecolor='black', ecolor=c_data,
                 label='Experimental Data', zorder=10)

for i in range(len(all_individual_data)):
    ax2.plot(theory_angles, optimal_norm * all_individual_data[i], 
             '-', color=c_individual, linewidth=0.3, alpha=0.3, zorder=1)

ax2.plot(theory_angles, optimal_norm * theory_mean, '-', color=c_theory, linewidth=3, 
         label=f'Mean Theory × {optimal_norm:.2f}', zorder=5)

ax2.set_yscale('log')
ax2.set_ylabel('Cross Section (mb/sr)', fontsize=12, fontweight='bold', color=c_weight)
ax2.set_title(f'All {len(all_individual_data)} Individual KDUQ Calculations', 
              fontsize=12, fontweight='bold', color=c_weight)
ax2.legend(fontsize=11, loc='upper right', framealpha=1, edgecolor=c_weight)
ax2.grid(True, which='both', color=c_grid, linestyle='--', linewidth=0.5)
ax2.set_xlim(0, 60)
if has_exp_data:
    ax2.tick_params(labelbottom=False, colors=c_weight, which='both')
else:
    ax2.set_xlabel('Angle (degrees)', fontsize=12, fontweight='bold', color=c_weight)
    ax2.tick_params(colors=c_weight, which='both')
for spine in ax2.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)

if has_exp_data:
    # Panel 3: Residuals
    ax3.errorbar(exp_angles, normalized_residuals, yerr=1.0,
                 fmt='^', color=c_resid, markersize=8, capsize=5, capthick=2,
                 markeredgecolor='black', ecolor=c_resid)
    ax3.axhline(y=0, color='black', linestyle='-', linewidth=2)
    ax3.axhline(y=2, color=c_data, linestyle='--', linewidth=2, alpha=0.5)
    ax3.axhline(y=-2, color=c_data, linestyle='--', linewidth=2, alpha=0.5)
    ax3.set_ylabel('Residuals (σ)', fontsize=12, fontweight='bold', color=c_weight)
    ax3.set_title('Residuals', fontsize=12, fontweight='bold', color=c_weight)
    ax3.grid(True, color=c_grid, linestyle='--', linewidth=0.5)
    ax3.set_xlim(0, 60)
    ax3.tick_params(labelbottom=False, colors=c_weight)
    for spine in ax3.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)
    
    # Panel 4: Error percentages
    ax4.bar(exp_angles, exp_error_percent, width=2, color=c_err, alpha=0.8, edgecolor='black', linewidth=1)
    ax4.set_ylabel('Error (%)', fontsize=12, fontweight='bold', color=c_weight)
    ax4.set_title('Percentage Errors', fontsize=12, fontweight='bold', color=c_weight)
    ax4.grid(True, axis='y', color=c_grid, linestyle='--', linewidth=0.5)
    ax4.set_xlim(0, 60)
    ax4.tick_params(labelbottom=False, colors=c_weight)
    for spine in ax4.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)
    
    # Panel 5: Weights
    ax5.bar(exp_angles, weights_normalized * 100, width=2, color=c_weight, alpha=0.8, edgecolor='black', linewidth=1)
    ax5.set_xlabel('Angle (degrees)', fontsize=12, fontweight='bold', color=c_weight)
    ax5.set_ylabel('Weight (%)', fontsize=12, fontweight='bold', color=c_weight)
    ax5.set_title('Normalized Weights (1/σ²)', fontsize=12, fontweight='bold', color=c_weight)
    ax5.grid(True, axis='y', color=c_grid, linestyle='--', linewidth=0.5)
    ax5.set_xlim(0, 60)
    ax5.tick_params(colors=c_weight)
    for spine in ax5.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)

plt.savefig(output_plot, dpi=300, bbox_inches='tight')
print(f"\nPlot saved as '{output_plot}'")

if has_exp_data:
    # Save fit results
    with open(output_txt, 'w') as f:
        f.write(f"FIT RESULTS FOR STATE {state_id}\\n")
        f.write("=" * 80 + "\\n")
        f.write(f"Optimal normalization: {optimal_norm:.4f} ± {norm_uncertainty:.4f}\\n")
        f.write(f"Chi-squared (χ²): {min_chi_squared:.4f}\\n")
        f.write(f"Reduced chi-squared (χ²/ν): {reduced_chi_squared:.4f}\\n")
        f.write("=" * 80 + "\\n")
    print(f"Fit results saved to '{output_txt}'")
