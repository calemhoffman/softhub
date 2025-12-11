#!/usr/bin/env python3
"""
Aggregates independent fit results from all states into a single summary file.
Reads Results/fit_state*.txt and writes Results/independent_fits_summary.txt
"""
import glob
import os
import re

RESULTS_DIR = "Results"
OUTPUT_FILE = f"{RESULTS_DIR}/independent_fits_summary.txt"

def parse_fit_file(filepath):
    """Extracts N, Chi2, RedChi2 from a fit result text file."""
    data = {}
    with open(filepath, 'r') as f:
        content = f.read()
        
        # Parse Normalization: "Optimal normalization: 16.2291 ± 0.9426"
        norm_match = re.search(r"Optimal normalization: ([0-9.]+) ± ([0-9.]+)", content)
        if norm_match:
            data['N'] = float(norm_match.group(1))
            data['N_err'] = float(norm_match.group(2))
        else:
            data['N'] = 0.0
            data['N_err'] = 0.0
            
        # Parse Chi-squared: "Chi-squared (χ²): 3.8397"
        chi2_match = re.search(r"Chi-squared \(χ²\): ([0-9.]+)", content)
        if chi2_match:
            data['Chi2'] = float(chi2_match.group(1))
        else:
            data['Chi2'] = 0.0

        # Parse Reduced Chi-squared: "Reduced chi-squared (χ²/ν): 0.4800"
        redchi2_match = re.search(r"Reduced chi-squared \(χ²/ν\): ([0-9.]+)", content)
        if redchi2_match:
            data['RedChi2'] = float(redchi2_match.group(1))
        else:
            data['RedChi2'] = 0.0
            
    return data

def main():
    print(f"Collecting independent fit results from {RESULTS_DIR}...")
    
    summary_data = []
    
    # Loop through states 1-9
    for i in range(1, 10):
        fname = f"{RESULTS_DIR}/fit_state{i}.txt"
        if os.path.exists(fname):
            res = parse_fit_file(fname)
            res['state_id'] = i
            summary_data.append(res)
        else:
            print(f"Warning: Results for state {i} not found ({fname})")

    if not summary_data:
        print("No fit result files found!")
        return

    # Write summary
    with open(OUTPUT_FILE, 'w') as f:
        f.write("# Independent Fit Summary\n")
        f.write(f"{'State':<6} {'N':<10} {'N_err':<10} {'Chi2':<10} {'RedChi2':<10}\n")
        f.write("-" * 50 + "\n")
        
        print(f"\n{'State':<6} {'N':<10} {'N_err':<10} {'RedChi2':<10}")
        print("-" * 40)
        
        for state in summary_data:
            line = f"{state.get('state_id'):<6d} {state.get('N'):<10.4f} {state.get('N_err'):<10.4f} {state.get('Chi2'):<10.4f} {state.get('RedChi2'):<10.4f}\n"
            f.write(line)
            print(f"{state.get('state_id'):<6d} {state.get('N'):<10.4f} {state.get('N_err'):<10.4f} {state.get('RedChi2'):<10.4f}")

    print(f"\nSummary saved to {OUTPUT_FILE}")

if __name__ == "__main__":
    main()
