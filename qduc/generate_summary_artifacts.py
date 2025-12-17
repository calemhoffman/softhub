#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import re
import os

# Configuration
RESULTS_FILE = 'Results/independent_fits_summary.txt'
CONFIG_FILE = 'states.config'
OUTPUT_LATEX = 'Results/fit_results_table.tex'
OUTPUT_PLOT = 'Results/normalized_spectroscopic_factors.png'

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
            
            # Calculate Ex assuming State 1 is Ground State
            # Store temporarily, will adjust if State 1 isn't first processed
            states[sid] = {
                'Q': q_val,
                'L': l_trans,
                'J': j_trans,
                'Ex': 0.0 # Placeholder
            }
    
    # Calculate Ex based on State 1 (Ground State)
    if 1 in states:
        gs_q = states[1]['Q']
        for sid in states:
            states[sid]['Ex'] = gs_q - states[sid]['Q']
            
    return states

def parse_results():
    data = []
    with open(RESULTS_FILE, 'r') as f:
        # Skip header lines
        lines = f.readlines()
        start_idx = 0
        for i, line in enumerate(lines):
            if line.startswith('---'):
                start_idx = i + 1
                break
        
        for line in lines[start_idx:]:
            if not line.strip(): continue
            parts = line.split()
            if len(parts) >= 5:
                data.append({
                    'id': int(parts[0]),
                    'N': float(parts[1]),
                    'N_err': float(parts[2]),
                    'Chi2': float(parts[3]),
                    'RedChi2': float(parts[4])
                })
    return pd.DataFrame(data)
def parse_csv_data():
    """Parses the c2s_good_only.csv file."""
    if not os.path.exists('c2s_good_only.csv'):
        print("Warning: c2s_good_only.csv not found.")
        return pd.DataFrame()
    return pd.read_csv('c2s_good_only.csv')

def generate_latex(df, states):
    with open(OUTPUT_LATEX, 'w') as f:
        f.write(r'\begin{table}[h]' + '\n')
        f.write(r'\centering' + '\n')
        f.write(r'\caption{Summary of independent fits for ${}^{36}$S(d,p)${}^{37}$S reactions.}' + '\n')
        f.write(r'\begin{tabular}{c c c c c c}' + '\n')
        f.write(r'\hline\hline' + '\n')
        f.write(r'State & $E_x$ (MeV) & $L$ & $J^\pi$ & $S$ (Rel.) & $\chi^2/\nu$ \\' + '\n')
        f.write(r'\hline' + '\n')
        
        # Determine normalization factor (State 1, L=3)
        # Using 34S(d,p)37S? No, looks like 36S(d,p)37S based on target mass 36.
        # N is proportional to S (Spectroscopic Factor).
        # We normalize to State 1.
        
        norm_row = df[df['id'] == 1].iloc[0]
        norm_factor = norm_row['N']
        if norm_factor == 0: norm_factor = 1.0 # Avoid division by zero
        
        for _, row in df.iterrows():
            sid = int(row['id'])
            if sid not in states: continue
            
            st = states[sid]
            
            # Normalize S
            s_rel = row['N'] / norm_factor
            s_err_rel = s_rel * np.sqrt((row['N_err']/row['N'])**2 + (norm_row['N_err']/norm_row['N'])**2)
            
            # Format J
            j_str = f"{st['J']}"
            if j_str.endswith('.5'):
                j_str = f"{int(st['J']*2)}/2"
            
            # Add parity if L comes from config
            parity = "+" if st['L'] % 2 == 0 else "-"
            
            line = f"{sid} & {st['Ex']:.3f} & {st['L']} & {j_str}^{{{parity}}} & {s_rel:.3f} $\\pm$ {s_err_rel:.3f} & {row['RedChi2']:.2f} \\\\"
            f.write(line + '\n')
            
        f.write(r'\hline\hline' + '\n')
        f.write(r'\end{tabular}' + '\n')
        f.write(r'\label{tab:sf_results}' + '\n')
        f.write(r'\end{table}' + '\n')
    
    print(f"LaTeX table generated at {OUTPUT_LATEX}")

def generate_plot(df, states):
    # Prepare data for plotting
    plot_data = []
    
    norm_row = df[df['id'] == 1].iloc[0]
    norm_factor = norm_row['N']
    if norm_factor == 0: norm_factor = 1.0
    
    for _, row in df.iterrows():
        sid = int(row['id'])
        if sid not in states: continue
        st = states[sid]
        
        s_rel = row['N'] / norm_factor
        s_err_rel = s_rel * np.sqrt((row['N_err']/row['N'])**2 + (norm_row['N_err']/norm_row['N'])**2)
        
        plot_data.append({
            'Ex': st['Ex'],
            'S_rel': s_rel,
            'S_err': s_err_rel,
            'L': st['L']
        })
    
    pdf = pd.DataFrame(plot_data)
    
    plt.figure(figsize=(10, 6))
    
    # Plot different L-transfers with different markers/colors
    markers = {0: 'o', 1: 's', 2: '^', 3: 'D', 4: 'v'}
    colors = {0: 'black', 1: 'blue', 2: 'green', 3: 'red', 4: 'purple'}
    
    for l_val in sorted(pdf['L'].unique()):
        subset = pdf[pdf['L'] == l_val]
        plt.errorbar(subset['Ex'], subset['S_rel'], yerr=subset['S_err'], 
                     fmt=markers.get(l_val, 'o'), 
                     color=colors.get(l_val, 'black'),
                     capsize=5, markersize=10, 
                     label=f'L={l_val}')
    
    # Plot CSV data if available
    csv_df = parse_csv_data()
    if not csv_df.empty:
        # Normalize CSV data to its own ground state (first row or energy=0)
        # Assuming the specific row structure or sorting by energy
        csv_df = csv_df.sort_values('energy')
        
        # Find normalization factor from the first state (Energy=0)
        norm_row_csv = csv_df[csv_df['energy'] == 0.0]
        if not norm_row_csv.empty:
            csv_norm_factor = norm_row_csv.iloc[0]['c2s']
        else:
            csv_norm_factor = 1.0 # Fallback
            
        if csv_norm_factor == 0: csv_norm_factor = 1.0

        for l_val in sorted(csv_df['ell'].unique()):
            subset = csv_df[csv_df['ell'] == l_val]
            
            s_rel_csv = subset['c2s'] / csv_norm_factor
            
            # Apply correction factor for j=l-1/2 states (ell=1, j=0.5)
            # Per user specification, these need a factor of 2
            correction_factor = subset.apply(
                lambda row: 2.0 if (row['ell'] == 1 and row['spin'] == 0.5) else 1.0, 
                axis=1
            )
            s_rel_csv = s_rel_csv * correction_factor
            
            # Error propagation for relative value
            # s_err_rel = s_rel * sqrt((err/val)^2 + (norm_err/norm_val)^2)
            norm_err = norm_row_csv.iloc[0]['c2s_err'] if not norm_row_csv.empty else 0.0
            
            s_err_rel_csv = s_rel_csv * np.sqrt((subset['c2s_err']/subset['c2s'])**2 + (norm_err/csv_norm_factor)**2)

            plt.errorbar(subset['energy'], s_rel_csv, yerr=s_err_rel_csv,
                         fmt=markers.get(int(l_val), 'o'),
                         color=colors.get(int(l_val), 'black'),
                         markerfacecolor='none',  # Open symbols
                         markeredgewidth=1.5,
                         capsize=5, markersize=10,
                         linestyle='None',
                         label=f'Lit. L={int(l_val)}' if f'L={int(l_val)}' not in plt.gca().get_legend_handles_labels()[1] else "")

    plt.xlabel('Excitation Energy ($E_x$) [MeV]', fontsize=14)
    plt.ylabel('Relative Spectroscopic Factor ($S / S_{gs}$)', fontsize=14)
    plt.title('Spectroscopic Factors Normalized to Ground State ($37S$)', fontsize=16)
    plt.grid(True, linestyle='--', alpha=0.7)
    
    # Handle duplicate labels in legend
    handles, labels = plt.gca().get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    plt.legend(by_label.values(), by_label.keys(), fontsize=12)
    
    plt.axhline(1.0, color='gray', linestyle=':', alpha=0.5)
    
    plt.tight_layout()
    plt.savefig(OUTPUT_PLOT, dpi=300)
    print(f"Plot generated at {OUTPUT_PLOT}")

def main():
    print("Parsing configuration...")
    states = parse_config()
    
    print("Parsing results...")
    df = parse_results()
    
    print("Generating artifacts...")
    generate_latex(df, states)
    generate_plot(df, states)
    
    print("Done.")

if __name__ == "__main__":
    main()
