#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt

# Load experimental data
exp_data = np.loadtxt('experimental_data.dat')
exp_angles = exp_data[:, 0]
exp_cross_section = exp_data[:, 1]
exp_errors = exp_data[:, 2]

# Load theoretical predictions from mean1.py output
theory_data = np.loadtxt('output.dat')
theory_angles = theory_data[:, 0]
theory_mean = theory_data[:, 1]
theory_plus1sigma = theory_data[:, 2]
theory_minus1sigma = theory_data[:, 4]

# Interpolate theoretical values at experimental angles
theory_at_exp = np.interp(exp_angles, theory_angles, theory_mean)

# Calculate chi-squared
chi_squared = np.sum(((exp_cross_section - theory_at_exp) / exp_errors)**2)
reduced_chi_squared = chi_squared / (len(exp_angles) - 1)  # degrees of freedom = N - 1

# Calculate residuals
residuals = exp_cross_section - theory_at_exp
normalized_residuals = residuals / exp_errors

# Print fit statistics
print("=" * 60)
print("FIT STATISTICS")
print("=" * 60)
print(f"Number of data points: {len(exp_angles)}")
print(f"Chi-squared (χ²): {chi_squared:.4f}")
print(f"Reduced chi-squared (χ²/ν): {reduced_chi_squared:.4f}")
print(f"Degrees of freedom (ν): {len(exp_angles) - 1}")
print("=" * 60)
print("\nDETAILED COMPARISON:")
print("-" * 60)
print(f"{'Angle':>6} {'Exp':>10} {'Error':>10} {'Theory':>10} {'Residual':>10} {'σ':>6}")
print("-" * 60)
for i in range(len(exp_angles)):
    print(f"{exp_angles[i]:6.1f} {exp_cross_section[i]:10.2f} {exp_errors[i]:10.2f} "
          f"{theory_at_exp[i]:10.2f} {residuals[i]:10.2f} {normalized_residuals[i]:6.2f}")
print("-" * 60)

# Create visualization
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 10), height_ratios=[3, 1])

# Top panel: Data vs Theory
ax1.errorbar(exp_angles, exp_cross_section, yerr=exp_errors, 
             fmt='o', color='red', markersize=8, capsize=5, capthick=2,
             label='Experimental Data', zorder=3)

ax1.plot(theory_angles, theory_mean, 'b-', linewidth=2, label='Theory (Mean)', zorder=2)
ax1.fill_between(theory_angles, theory_minus1sigma, theory_plus1sigma, 
                  alpha=0.3, color='blue', label='Theory ±1σ', zorder=1)

ax1.set_xlabel('Angle (degrees)', fontsize=12)
ax1.set_ylabel('Cross Section (mb/sr)', fontsize=12)
ax1.set_title(f'Experimental Data vs Theory\nχ²/ν = {reduced_chi_squared:.3f}', fontsize=14)
ax1.legend(fontsize=11)
ax1.grid(True, alpha=0.3)
ax1.set_xlim(0, 60)

# Bottom panel: Residuals
ax2.errorbar(exp_angles, normalized_residuals, yerr=1.0,
             fmt='o', color='red', markersize=8, capsize=5, capthick=2)
ax2.axhline(y=0, color='black', linestyle='-', linewidth=1)
ax2.axhline(y=2, color='gray', linestyle='--', linewidth=1, alpha=0.5)
ax2.axhline(y=-2, color='gray', linestyle='--', linewidth=1, alpha=0.5)

ax2.set_xlabel('Angle (degrees)', fontsize=12)
ax2.set_ylabel('Normalized Residuals (σ)', fontsize=12)
ax2.set_title('Residuals', fontsize=12)
ax2.grid(True, alpha=0.3)
ax2.set_xlim(0, 60)

plt.tight_layout()
plt.savefig('fit_comparison.png', dpi=300, bbox_inches='tight')
print(f"\nPlot saved as 'fit_comparison.png'")

# Save fit results to file
with open('fit_results.txt', 'w') as f:
    f.write("FIT STATISTICS\n")
    f.write("=" * 60 + "\n")
    f.write(f"Number of data points: {len(exp_angles)}\n")
    f.write(f"Chi-squared (χ²): {chi_squared:.4f}\n")
    f.write(f"Reduced chi-squared (χ²/ν): {reduced_chi_squared:.4f}\n")
    f.write(f"Degrees of freedom (ν): {len(exp_angles) - 1}\n")
    f.write("=" * 60 + "\n\n")
    f.write("DETAILED COMPARISON:\n")
    f.write("-" * 60 + "\n")
    f.write(f"{'Angle':>6} {'Exp':>10} {'Error':>10} {'Theory':>10} {'Residual':>10} {'σ':>6}\n")
    f.write("-" * 60 + "\n")
    for i in range(len(exp_angles)):
        f.write(f"{exp_angles[i]:6.1f} {exp_cross_section[i]:10.2f} {exp_errors[i]:10.2f} "
                f"{theory_at_exp[i]:10.2f} {residuals[i]:10.2f} {normalized_residuals[i]:6.2f}\n")
    f.write("-" * 60 + "\n")

print(f"Fit results saved to 'fit_results.txt'")
