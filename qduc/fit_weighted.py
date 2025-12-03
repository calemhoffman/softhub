#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize_scalar

# Load experimental data
exp_data = np.loadtxt('experimental_data.dat')
exp_angles = exp_data[:, 0]
exp_cross_section = exp_data[:, 1]
exp_errors = exp_data[:, 2]

# Calculate percentage errors and weights
exp_error_percent = (exp_errors / exp_cross_section) * 100
weights = 1.0 / (exp_errors**2)  # Inverse variance weighting
weights_normalized = weights / np.sum(weights)  # Normalized weights

# Load theoretical predictions from mean1.py output
theory_data = np.loadtxt('output.dat')
theory_angles = theory_data[:, 0]
theory_mean = theory_data[:, 1]
theory_plus1sigma = theory_data[:, 2]
theory_minus1sigma = theory_data[:, 4]

# Interpolate theoretical values at experimental angles
theory_at_exp = np.interp(exp_angles, theory_angles, theory_mean)

# Define weighted chi-squared function as a function of normalization factor
def chi_squared_func(norm_factor):
    normalized_theory = norm_factor * theory_at_exp
    chi_sq = np.sum(weights * (exp_cross_section - normalized_theory)**2)
    return chi_sq

# Find optimal normalization factor
result = minimize_scalar(chi_squared_func, bounds=(0.1, 100), method='bounded')
optimal_norm = result.x
min_chi_squared = result.fun
reduced_chi_squared = min_chi_squared / (len(exp_angles) - 1)  # N - 1 free parameters

# Calculate normalized theory values
theory_normalized = optimal_norm * theory_at_exp
residuals = exp_cross_section - theory_normalized
normalized_residuals = residuals / exp_errors

# Calculate uncertainty on normalization factor using chi-squared + 1 method
def find_norm_uncertainty():
    """Find normalization uncertainty where chi^2 = chi^2_min + 1"""
    target_chi_sq = min_chi_squared + 1
    
    # Search for upper bound
    norm_upper = optimal_norm
    for factor in np.linspace(optimal_norm, optimal_norm * 2, 100):
        if chi_squared_func(factor) >= target_chi_sq:
            norm_upper = factor
            break
    
    # Search for lower bound
    norm_lower = optimal_norm
    for factor in np.linspace(optimal_norm, optimal_norm * 0.5, 100):
        if chi_squared_func(factor) >= target_chi_sq:
            norm_lower = factor
            break
    
    return (norm_upper - optimal_norm + optimal_norm - norm_lower) / 2

norm_uncertainty = find_norm_uncertainty()

# Print fit statistics
print("=" * 80)
print("FIT WITH NORMALIZATION FACTOR (WEIGHTED BY ERROR)")
print("=" * 80)
print(f"Optimal normalization factor: {optimal_norm:.4f} ± {norm_uncertainty:.4f}")
print(f"Number of data points: {len(exp_angles)}")
print(f"Chi-squared (χ²): {min_chi_squared:.4f}")
print(f"Reduced chi-squared (χ²/ν): {reduced_chi_squared:.4f}")
print(f"Degrees of freedom (ν): {len(exp_angles) - 1}")
print("=" * 80)
print("\nWEIGHTING INFORMATION:")
print("-" * 80)
print(f"{'Angle':>6} {'Error %':>10} {'Weight':>12} {'Norm Weight':>12}")
print("-" * 80)
for i in range(len(exp_angles)):
    print(f"{exp_angles[i]:6.1f} {exp_error_percent[i]:10.2f} {weights[i]:12.6f} {weights_normalized[i]:12.6f}")
print("-" * 80)
print(f"\nNote: Weights = 1/σ² (inverse variance weighting)")
print(f"Points with smaller errors have higher weights in the fit.\n")

print("\nDETAILED COMPARISON:")
print("-" * 80)
print(f"{'Angle':>6} {'Exp':>10} {'Error':>10} {'Error%':>8} {'Theory':>10} {'Resid':>10} {'σ':>6}")
print("-" * 80)
for i in range(len(exp_angles)):
    print(f"{exp_angles[i]:6.1f} {exp_cross_section[i]:10.2f} {exp_errors[i]:10.2f} "
          f"{exp_error_percent[i]:7.1f}% {theory_normalized[i]:10.2f} "
          f"{residuals[i]:10.2f} {normalized_residuals[i]:6.2f}")
print("-" * 80)

# Create visualization
# 90s Theme Colors
c_bg = '#FFFFFF'       # White background
c_data = '#FF00FF'     # Magenta (Hot Pink)
c_theory = '#00CED1'   # Dark Turquoise
c_fill = '#FFD700'     # Gold/Yellow
c_resid = '#32CD32'    # Lime Green
c_err = '#FF4500'      # Orange Red
c_weight = '#9400D3'   # Dark Violet
c_grid = '#E0E0E0'     # Light Gray

plt.rcParams['font.family'] = 'monospace' # Retro font feel

fig = plt.figure(figsize=(14, 11), facecolor=c_bg)
gs = fig.add_gridspec(4, 1, height_ratios=[3, 1, 1, 1], hspace=0.3)
ax1 = fig.add_subplot(gs[0])
ax2 = fig.add_subplot(gs[1])
ax3 = fig.add_subplot(gs[2])
ax4 = fig.add_subplot(gs[3])

# Top panel: Data vs Normalized Theory (Log Scale)
ax1.errorbar(exp_angles, exp_cross_section, yerr=exp_errors, 
             fmt='s', color=c_data, markersize=8, capsize=5, capthick=2,
             markeredgecolor='black', ecolor=c_data,
             label='Experimental Data', zorder=3)

ax1.plot(theory_angles, optimal_norm * theory_mean, '-', color=c_theory, linewidth=3, 
         label=f'Theory × {optimal_norm:.2f}', zorder=2)
ax1.fill_between(theory_angles, 
                  optimal_norm * theory_minus1sigma, 
                  optimal_norm * theory_plus1sigma, 
                  alpha=0.4, color=c_fill, label='Theory ±1σ', zorder=1)

ax1.set_yscale('log') # Log scale requested
ax1.set_ylabel('Cross Section (mb/sr)', fontsize=12, fontweight='bold', color=c_weight)
ax1.set_title(f'Experimental Data vs Normalized Theory (Weighted Fit)\n' + 
              f'Normalization = {optimal_norm:.3f} ± {norm_uncertainty:.3f}, χ²/ν = {reduced_chi_squared:.3f}', 
              fontsize=14, fontweight='bold', color=c_weight)
ax1.legend(fontsize=11, loc='upper right', framealpha=1, edgecolor=c_weight)
ax1.grid(True, which='both', color=c_grid, linestyle='--', linewidth=0.5)
ax1.set_xlim(0, 60)
ax1.tick_params(labelbottom=False, colors=c_weight, which='both')
for spine in ax1.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)

# Second panel: Residuals
ax2.errorbar(exp_angles, normalized_residuals, yerr=1.0,
             fmt='^', color=c_resid, markersize=8, capsize=5, capthick=2,
             markeredgecolor='black', ecolor=c_resid)
ax2.axhline(y=0, color='black', linestyle='-', linewidth=2)
ax2.axhline(y=2, color=c_data, linestyle='--', linewidth=2, alpha=0.5)
ax2.axhline(y=-2, color=c_data, linestyle='--', linewidth=2, alpha=0.5)

ax2.set_ylabel('Residuals (σ)', fontsize=12, fontweight='bold', color=c_weight)
ax2.set_title('Residuals', fontsize=12, fontweight='bold', color=c_weight)
ax2.grid(True, color=c_grid, linestyle='--', linewidth=0.5)
ax2.set_xlim(0, 60)
ax2.tick_params(labelbottom=False, colors=c_weight)
for spine in ax2.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)

# Third panel: Error percentages
ax3.bar(exp_angles, exp_error_percent, width=2, color=c_err, alpha=0.8, edgecolor='black', linewidth=1)
ax3.set_ylabel('Error (%)', fontsize=12, fontweight='bold', color=c_weight)
ax3.set_title('Percentage Errors', fontsize=12, fontweight='bold', color=c_weight)
ax3.grid(True, axis='y', color=c_grid, linestyle='--', linewidth=0.5)
ax3.set_xlim(0, 60)
ax3.tick_params(labelbottom=False, colors=c_weight)
for spine in ax3.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)

# Fourth panel: Normalized weights
ax4.bar(exp_angles, weights_normalized * 100, width=2, color=c_weight, alpha=0.8, edgecolor='black', linewidth=1)
ax4.set_xlabel('Angle (degrees)', fontsize=12, fontweight='bold', color=c_weight)
ax4.set_ylabel('Weight (%)', fontsize=12, fontweight='bold', color=c_weight)
ax4.set_title('Normalized Weights (1/σ²)', fontsize=12, fontweight='bold', color=c_weight)
ax4.grid(True, axis='y', color=c_grid, linestyle='--', linewidth=0.5)
ax4.set_xlim(0, 60)
ax4.tick_params(colors=c_weight)
for spine in ax4.spines.values(): spine.set_color(c_weight); spine.set_linewidth(2)

plt.savefig('fit_weighted.png', dpi=300, bbox_inches='tight')
print(f"\nPlot saved as 'fit_weighted.png'")

# Save fit results to file
with open('fit_weighted.txt', 'w') as f:
    f.write("FIT WITH NORMALIZATION FACTOR (WEIGHTED BY ERROR)\n")
    f.write("=" * 80 + "\n")
    f.write(f"Optimal normalization factor: {optimal_norm:.4f} ± {norm_uncertainty:.4f}\n")
    f.write(f"Number of data points: {len(exp_angles)}\n")
    f.write(f"Chi-squared (χ²): {min_chi_squared:.4f}\n")
    f.write(f"Reduced chi-squared (χ²/ν): {reduced_chi_squared:.4f}\n")
    f.write(f"Degrees of freedom (ν): {len(exp_angles) - 1}\n")
    f.write("=" * 80 + "\n\n")
    
    f.write("WEIGHTING INFORMATION:\n")
    f.write("-" * 80 + "\n")
    f.write(f"{'Angle':>6} {'Error %':>10} {'Weight':>12} {'Norm Weight':>12}\n")
    f.write("-" * 80 + "\n")
    for i in range(len(exp_angles)):
        f.write(f"{exp_angles[i]:6.1f} {exp_error_percent[i]:10.2f} {weights[i]:12.6f} {weights_normalized[i]:12.6f}\n")
    f.write("-" * 80 + "\n")
    f.write(f"\nNote: Weights = 1/σ² (inverse variance weighting)\n")
    f.write(f"Points with smaller errors have higher weights in the fit.\n\n")
    
    f.write("DETAILED COMPARISON:\n")
    f.write("-" * 80 + "\n")
    f.write(f"{'Angle':>6} {'Exp':>10} {'Error':>10} {'Error%':>8} {'Theory':>10} {'Resid':>10} {'σ':>6}\n")
    f.write("-" * 80 + "\n")
    for i in range(len(exp_angles)):
        f.write(f"{exp_angles[i]:6.1f} {exp_cross_section[i]:10.2f} {exp_errors[i]:10.2f} "
                f"{exp_error_percent[i]:7.1f}% {theory_normalized[i]:10.2f} "
                f"{residuals[i]:10.2f} {normalized_residuals[i]:6.2f}\n")
    f.write("-" * 80 + "\n")

print(f"Fit results saved to 'fit_weighted.txt'")
