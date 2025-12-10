#!/usr/bin/env python3
"""
Plot multi-state QDUC results
Combines all states in a single plot
"""
import numpy as np
import matplotlib.pyplot as plt
import os
import sys

CONFIG_FILE = "states.config"
RESULTS_DIR = "Results"

# 90s Theme Colors - more colors for multiple states
colors = ['#00CED1', '#FF00FF', '#32CD32', '#FF4500', '#FFD700', '#9400D3']
c_bg = '#FFFFFF'
c_weight = '#9400D3'
c_grid = '#E0E0E0'

plt.rcParams['font.family'] = 'monospace'

print("=" * 70)
print("Plotting Multi-State QDUC Results")
print("=" * 70)
print("")

# Read config file to get state information
states = []
with open(CONFIG_FILE, 'r') as f:
    for line in f:
        line = line.strip()
        if line.startswith('#') or not line:
            continue
        parts = [p.strip() for p in line.split('|')]
        state_id = parts[0]
        q_value = parts[1]
        l_transfer = parts[2]
        j_transfer = parts[3]
        description = parts[9] if len(parts) > 9 else f"state_{state_id}"
        states.append({
            'id': state_id,
            'q': q_value,
            'l': l_transfer,
            'j': j_transfer,
            'desc': description
        })

print(f"Found {len(states)} active states")
print("")

# Create figure
fig, ax = plt.subplots(figsize=(14, 9), facecolor=c_bg)

# Plot each state
for i, state in enumerate(states):
    state_id = state['id']
    output_file = f"{RESULTS_DIR}/output_state{state_id}.dat"
    
    if not os.path.exists(output_file):
        print(f"WARNING: Results file not found: {output_file}")
        continue
    
    # Load data
    data = np.loadtxt(output_file)
    angles = data[:, 0]
    mean = data[:, 1]
    plus1 = data[:, 2]
    minus1 = data[:, 4]
    
    # Choose color
    color = colors[i % len(colors)]
    
    # Create label
    label = f"State {state_id}: Q={state['q']} MeV, (l,j)=({state['l']},{state['j']})"
    
    # Plot mean with uncertainty band
    ax.fill_between(angles, minus1, plus1, alpha=0.2, color=color)
    ax.plot(angles, mean, '-', color=color, linewidth=2.5, label=label, zorder=3)
    
    print(f"✓ Plotted State {state_id}: {state['desc']}")

ax.set_yscale('log')
ax.set_xlabel('Angle (degrees)', fontsize=14, fontweight='bold', color=c_weight)
ax.set_ylabel('Cross Section (mb/sr)', fontsize=14, fontweight='bold', color=c_weight)
ax.set_title('Multi-State QDUC Predictions\n¹⁹O(d,p)²⁰O - KDUQ Democratic Potential', 
             fontsize=16, fontweight='bold', color=c_weight)
ax.legend(fontsize=11, loc='best', framealpha=1, edgecolor=c_weight)
ax.grid(True, which='both', color=c_grid, linestyle='--', linewidth=0.5)
ax.set_xlim(0, 180)
ax.tick_params(colors=c_weight, which='both', labelsize=11)
for spine in ax.spines.values(): 
    spine.set_color(c_weight)
    spine.set_linewidth(2)

plt.tight_layout()
output_plot = f"{RESULTS_DIR}/multistate_comparison.png"
plt.savefig(output_plot, dpi=300, bbox_inches='tight')

print("")
print("=" * 70)
print(f"Plot saved to: {output_plot}")
print("=" * 70)
