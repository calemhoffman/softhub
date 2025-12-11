#!/usr/bin/env python3
"""
Visualize the multi-state fit results in a 9-panel grid.
"""
import numpy as np
import matplotlib.pyplot as plt
import glob
import os
import sys

# Configuration
CONFIG_FILE = "states.config"
RESULTS_DIR = "Results"
EXP_DATA_DIR = "experimental_data_bylevel"
GLOBAL_RESULTS_FILE = "Results/global_fit_results.txt"
OUTPUT_PLOT = "Results/multistate_fit_summary.png"

def load_normalization():
    """Load global normalization factor from fit results output"""
    if not os.path.exists(GLOBAL_RESULTS_FILE):
        print(f"Warning: {GLOBAL_RESULTS_FILE} not found. Using N=1.0")
        return 1.0, 0.0
    
    norm = 1.0
    unc = 0.0
    with open(GLOBAL_RESULTS_FILE, 'r') as f:
        for line in f:
            if line.startswith("Normalization:"):
                norm = float(line.split(':')[1].strip())
            if line.startswith("Norm_Uncertainty:"):
                unc = float(line.split(':')[1].strip())
    return norm, unc

def load_states():
    states = []
    with open(CONFIG_FILE, 'r') as f:
        for line in f:
            if line.strip() and not line.strip().startswith('#'):
                parts = line.split('|')
                states.append({
                    'id': int(parts[0].strip()),
                    'energy': parts[1].strip(), # Q-value
                    'desc': parts[10].strip() if len(parts) > 10 else f"State {parts[0]}"
                })
    return states

def load_data(state_id):
    # Experiment
    exp_files = glob.glob(f"{EXP_DATA_DIR}/level_{state_id}_*.dat")
    exp_data = None
    if exp_files:
        exp_data = np.loadtxt(exp_files[0])
        
    # Theory
    theory_file = f"{RESULTS_DIR}/output_state{state_id}.dat"
    theory_data = None
    if os.path.exists(theory_file):
        theory_data = np.loadtxt(theory_file)
        
    return exp_data, theory_data

def plot_all():
    norm, norm_unc = load_normalization()
    print(f"Using Global Normalization: {norm:.4f} ± {norm_unc:.4f}")
    
    states = load_states()
    
    # Setup 3x3 Grid (for 9 states)
    fig, axes = plt.subplots(3, 3, figsize=(18, 14), sharex=True)
    axes = axes.flatten()
    
    # Color scheme (90s style from user preference)
    c_bg = '#FFFFFF'
    c_data = '#FF00FF'   # Magenta
    c_theory = '#00CED1' # Dark Turquoise
    c_fill1 = '#FFD700'  # Gold
    c_fill2 = '#FF69B4'  # Hot Pink
    
    plt.rcParams['font.family'] = 'monospace'
    
    for i, ax in enumerate(axes):
        if i >= len(states):
            ax.axis('off')
            continue
            
        state = states[i]
        sid = state['id']
        desc = state['desc']
        
        # Load data
        exp, theory = load_data(sid)
        
        ax.set_title(f"State {sid}: {desc}", fontsize=10, fontweight='bold')
        ax.set_yscale('log')
        ax.grid(True, which='both', linestyle='--', alpha=0.5)
        
        if theory is not None:
            angle = theory[:, 0]
            mean = theory[:, 1] * norm
            p1 = theory[:, 2] * norm
            p2 = theory[:, 3] * norm
            m1 = theory[:, 4] * norm
            m2 = theory[:, 5] * norm
            
            # Plot bands
            ax.fill_between(angle, m2, p2, color=c_fill2, alpha=0.2, label='±2σ')
            ax.fill_between(angle, m1, p1, color=c_fill1, alpha=0.4, label='±1σ')
            ax.plot(angle, mean, color=c_theory, linewidth=2, label='Theory')
            
        if exp is not None:
            ax.errorbar(exp[:, 0], exp[:, 1], yerr=exp[:, 2], fmt='s', 
                       color=c_data, ecolor=c_data, markersize=4, 
                       capsize=2, label='Exp Data')
                       
        if i == 0:
            ax.legend(fontsize=8, loc='upper right')
            
    # Labels
    fig.text(0.5, 0.04, 'Center of Mass Angle (deg)', ha='center', fontsize=14)
    fig.text(0.04, 0.5, 'Cross Section (mb/sr)', va='center', rotation='vertical', fontsize=14)
    
    fig.suptitle(f"36S(d,p)37S Global Fit (N={norm:.2f})", fontsize=16, fontweight='bold')
    
    plt.savefig(OUTPUT_PLOT, dpi=300, bbox_inches='tight')
    print(f"Summary plot saved to {OUTPUT_PLOT}")

if __name__ == "__main__":
    plot_all()
