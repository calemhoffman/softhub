#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt

# Load theoretical predictions from mean1.py output
theory_data = np.loadtxt('output.dat')
theory_angles = theory_data[:, 0]
theory_mean = theory_data[:, 1]
theory_plus1sigma = theory_data[:, 2]
theory_plus2sigma = theory_data[:, 3]
theory_minus1sigma = theory_data[:, 4]
theory_minus2sigma = theory_data[:, 5]

# 90s Theme Colors
c_bg = '#FFFFFF'       # White background
c_theory = '#00CED1'   # Dark Turquoise
c_fill1 = '#FFD700'    # Gold/Yellow (1σ)
c_fill2 = '#FF69B4'    # Hot Pink (2σ)
c_weight = '#9400D3'   # Dark Violet
c_grid = '#E0E0E0'     # Light Gray

plt.rcParams['font.family'] = 'monospace'

fig, ax = plt.figure(figsize=(12, 8), facecolor=c_bg), plt.gca()

# Plot 2σ band
ax.fill_between(theory_angles, theory_minus2sigma, theory_plus2sigma, 
                 alpha=0.3, color=c_fill2, label='Theory ±2σ', zorder=1)

# Plot 1σ band
ax.fill_between(theory_angles, theory_minus1sigma, theory_plus1sigma, 
                 alpha=0.5, color=c_fill1, label='Theory ±1σ', zorder=2)

# Plot mean
ax.plot(theory_angles, theory_mean, '-', color=c_theory, linewidth=3, 
        label='Theory (Mean)', zorder=3)

ax.set_yscale('log')
ax.set_xlabel('Angle (degrees)', fontsize=14, fontweight='bold', color=c_weight)
ax.set_ylabel('Cross Section (mb/sr)', fontsize=14, fontweight='bold', color=c_weight)
ax.set_title('¹⁹O(d,p)²⁰O Theoretical Predictions\nKDUQ Democratic Potential (416 parameter sets)', 
             fontsize=16, fontweight='bold', color=c_weight)
ax.legend(fontsize=12, loc='upper right', framealpha=1, edgecolor=c_weight)
ax.grid(True, which='both', color=c_grid, linestyle='--', linewidth=0.5)
ax.set_xlim(0, 180)
ax.tick_params(colors=c_weight, which='both', labelsize=11)
for spine in ax.spines.values(): 
    spine.set_color(c_weight)
    spine.set_linewidth(2)

plt.tight_layout()
plt.savefig('theory_only.png', dpi=300, bbox_inches='tight')
print(f"\nTheory-only plot saved as 'theory_only.png'")
print(f"Angular range: {theory_angles[0]:.1f}° to {theory_angles[-1]:.1f}°")
print(f"Mean cross section range: {theory_mean.min():.6f} to {theory_mean.max():.6f} mb/sr")
print(f"Maximum ±1σ: {(theory_plus1sigma - theory_mean).max():.6f} mb/sr")
print(f"Maximum ±2σ: {(theory_plus2sigma - theory_mean).max():.6f} mb/sr")
