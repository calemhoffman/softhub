#!/usr/bin/env python3
"""
Generate a summary plot of Spectroscopic Factors vs Excitation Energy
Uses results from batch_fits_summary.txt and metadata from states.config
"""
import numpy as np
import matplotlib.pyplot as plt
import os
import re

def parse_energy(desc):
    """
    Extract energy in MeV from description like '37S_gs_0f7/2' (0 MeV) 
    or '37S_644_1p3/2' (0.644 MeV)
    """
    if "gs" in desc.lower():
        return 0.0
    # Search for a number following the first underscore
    match = re.search(r'_(\d+)_', desc)
    if match:
        return float(match.group(1)) / 1000.0 # keV to MeV
    return None

def main():
    summary_file = "Results/batch_fits_summary.txt"
    config_file = "states.config"
    output_plot = "Results/sf_summary_plot.png"

    if not os.path.exists(summary_file):
        print(f"Error: {summary_file} not found. Run batch_fit_states.py first.")
        return

    # 1. Read states.config for Excitation Energies
    energies = {}
    with open(config_file, 'r') as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith('#'):
                parts = [p.strip() for p in line.split('|')]
                sid = parts[0]
                desc = parts[10] if len(parts) > 10 else ""
                energy = parse_energy(desc)
                if energy is not None:
                    energies[sid] = energy

    # 2. Read SF values and uncertainties
    ids = []
    sf_values = []
    sf_errors = []
    ex_energies = []

    with open(summary_file, 'r') as f:
        for line in f:
            if "State" in line and "SF =" in line:
                # Format: State 1 : SF = 16.296045 ± 0.934723
                match = re.search(r'State\s+(\d+)\s*:\s*SF\s*=\s*([\d\.]+)\s*±\s*([\d\.]+)', line)
                if match:
                    sid = match.group(1)
                    val = float(match.group(2))
                    err = float(match.group(3))
                    
                    if sid in energies:
                        ids.append(sid)
                        sf_values.append(val)
                        sf_errors.append(err)
                        ex_energies.append(energies[sid])

    if not ids:
        print("No valid data found to plot.")
        return

    # 3. Visualization
    plt.figure(figsize=(12, 8))
    
    # Use log scale because SF range is large (~0.2 to ~16)
    plt.yscale('log')
    
    plt.errorbar(ex_energies, sf_values, yerr=sf_errors, fmt='bo', 
                 capsize=4, elinewidth=1, markeredgecolor='black', label='KDUQ RMS Fit')

    # Label points (optional, maybe just for gs)
    for i, sid in enumerate(ids):
        if float(ex_energies[i]) < 0.1: # Ground state
            plt.annotate(f"State {sid} (gs)", (ex_energies[i], sf_values[i]), 
                         textcoords="offset points", xytext=(0,10), ha='center', fontweight='bold')

    plt.xlabel('Excitation Energy $E_x$ (MeV)', fontsize=14, fontweight='bold')
    plt.ylabel('Spectroscopic Factor $C^2S$', fontsize=14, fontweight='bold')
    plt.title('Summary of Spectroscopic Factors for $^{36}$S(d,p)$^{37}$S\n(Statistical RMS Error from 416 Calculations)', 
              fontsize=16, fontweight='bold')
    
    plt.grid(True, which='both', linestyle='--', linewidth=0.5, alpha=0.7)
    plt.legend(fontsize=12)
    
    # Add a horizontal line at 1.0 (single particle limit reference)
    plt.axhline(y=1.0, color='gray', linestyle=':', alpha=0.5)

    plt.tight_layout()
    plt.savefig(output_plot, dpi=300)
    print(f"Summary plot saved to {output_plot}")

if __name__ == "__main__":
    main()
