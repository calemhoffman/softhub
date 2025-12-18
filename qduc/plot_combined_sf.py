#!/usr/bin/env python3
"""
Generate a combined summary plot of Spectroscopic Factors vs Excitation Energy
Includes:
1. Literature values (Open symbols)
2. Standard Independent Fits (Small dots)
3. New KDUQ Individual Fits with RMS Error (Filled symbols)
"""
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import re
import os

# Configuration
CONFIG_FILE = 'states.config'
LIT_FILE = 'c2s_good_only.csv'
STD_FILE = 'Results/independent_fits_summary.txt'
RMS_FILE = 'Results/batch_fits_summary.txt'
OUTPUT_PLOT = 'Results/combined_sf_plot.png'

def parse_config():
    states = {}
    with open(CONFIG_FILE, 'r') as f:
        for line in f:
            if line.strip().startswith('#') or not line.strip():
                continue
            parts = [p.strip() for p in line.split('|')]
            if len(parts) < 10: continue
            
            sid = int(parts[0])
            q_val = float(parts[1])
            l_trans = int(parts[2])
            j_trans = float(parts[3])
            
            states[sid] = {'Q': q_val, 'L': l_trans, 'J': j_trans, 'Ex': 0.0}
    
    if 1 in states:
        gs_q = states[1]['Q']
        for sid in states:
            states[sid]['Ex'] = gs_q - states[sid]['Q']
    return states

def parse_std_results():
    data = []
    if not os.path.exists(STD_FILE): return pd.DataFrame()
    with open(STD_FILE, 'r') as f:
        lines = f.readlines()
        start = 0
        for i, line in enumerate(lines):
            if line.startswith('---'): start = i + 1; break
        for line in lines[start:]:
            parts = line.split()
            if len(parts) >= 2:
                data.append({'id': int(parts[0]), 'val': float(parts[1]), 'err': float(parts[2])})
    return pd.DataFrame(data)

def parse_rms_results():
    data = []
    if not os.path.exists(RMS_FILE): return pd.DataFrame()
    with open(RMS_FILE, 'r') as f:
        for line in f:
            match = re.search(r'State\s+(\d+)\s*:\s*SF\s*=\s*([\d\.]+)\s*±\s*([\d\.]+)', line)
            if match:
                data.append({'id': int(match.group(1)), 'val': float(match.group(2)), 'err': float(match.group(3))})
    return pd.DataFrame(data)

def main():
    print("Parsing data sources...")
    states = parse_config()
    df_std = parse_std_results()
    df_rms = parse_rms_results()
    df_lit = pd.read_csv(LIT_FILE) if os.path.exists(LIT_FILE) else pd.DataFrame()

    plt.figure(figsize=(14, 10))
    
    markers = {0: 'o', 1: 's', 2: '^', 3: 'D', 4: 'v'}
    colors = {0: 'black', 1: 'blue', 2: 'green', 3: 'red', 4: 'purple'}

    # 1. Plot Standard Fits (Comparison)
    if not df_std.empty:
        gs_val = df_std[df_std['id'] == 1].iloc[0]['val']
        for i, (idx, row) in enumerate(df_std.iterrows()):
            id_val = int(row['id'])
            if id_val in states:
                ex = states[id_val]['Ex']
                l_val = states[id_val]['L']
                # Just plot without labels to avoid legend clutter
                plt.plot(ex, row['val']/gs_val, '.', color=colors.get(l_val, 'gray'), alpha=0.3, markersize=8)

    # 2. Plot Literature (Open symbols)
    if not df_lit.empty:
        # Normalize lit to its own gs
        gs_row_lit = df_lit[df_lit['energy'] == 0.0]
        if not gs_row_lit.empty:
            gs_val_lit = gs_row_lit.iloc[0]['c2s']
            for l_val in sorted(df_lit['ell'].unique()):
                subset = df_lit[df_lit['ell'] == l_val]
                vals = subset['c2s'] / gs_val_lit
                # Apply the specific j=1/2 correction rule from generate_summary_artifacts
                corrections = subset.apply(lambda r: 2.0 if (r['ell'] == 1 and r['spin'] == 0.5) else 1.0, axis=1)
                vals = vals * corrections
                plt.plot(subset['energy'], vals, markers.get(int(l_val), 'o'), 
                         markerfacecolor='none', markeredgecolor=colors.get(int(l_val), 'black'),
                         markersize=12, markeredgewidth=1.5, linestyle='None', label=f'Lit. L={int(l_val)}')

    # 3. Plot RMS Fits (Main Results - Filled symbols)
    if not df_rms.empty:
        gs_val_rms = df_rms[df_rms['id'] == 1].iloc[0]['val']
        for l_val in sorted(set(s['L'] for s in states.values())):
            subset_ids = [sid for sid, s in states.items() if s['L'] == l_val]
            subset_rms = df_rms[df_rms['id'].astype(int).isin(subset_ids)].copy()
            if subset_rms.empty: continue
            
            # Map Ex
            subset_rms['Ex'] = subset_rms['id'].astype(int).map(lambda x: states[x]['Ex'])
            subset_rms['RelVal'] = subset_rms['val'] / gs_val_rms
            # Propagate error
            gs_err = df_rms[df_rms['id'] == 1].iloc[0]['err']
            subset_rms['RelErr'] = subset_rms['RelVal'] * np.sqrt((subset_rms['err']/subset_rms['val'])**2 + (gs_err/gs_val_rms)**2)
            
            plt.errorbar(subset_rms['Ex'], subset_rms['RelVal'], yerr=subset_rms['RelErr'],
                         fmt=markers.get(l_val, 'o'), color=colors.get(l_val, 'black'),
                         markersize=10, capsize=5, elinewidth=2, label=f'Today\'s RMS L={l_val}')

    plt.yscale('log')
    plt.xlabel('Excitation Energy $E_x$ (MeV)', fontsize=15, fontweight='bold')
    plt.ylabel('Relative Spectroscopic Factor ($S / S_{gs}$)', fontsize=15, fontweight='bold')
    plt.title('Final SF Comparison for $^{36}$S(d,p)$^{37}$S\n(Normalized to Ground State)', fontsize=18, fontweight='bold')
    
    plt.grid(True, which='both', linestyle='--', alpha=0.5)
    
    # Legend management
    handles, labels = plt.gca().get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    plt.legend(by_label.values(), by_label.keys(), loc='lower left', fontsize=10, ncol=2)
    
    plt.axhline(1.0, color='red', linestyle=':', alpha=0.5, label='gs reference')
    plt.ylim(0.005, 2.5)
    plt.xlim(-0.2, 7.0)

    plt.tight_layout()
    plt.savefig(OUTPUT_PLOT, dpi=300)
    print(f"Combined plot generated at {OUTPUT_PLOT}")

if __name__ == "__main__":
    main()
