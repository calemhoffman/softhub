#!/usr/bin/env python3
"""
Batch process all states listed in states.config using fit_individual_kduq.py
"""
import os
import sys
import subprocess

CONFIG_FILE = "states.config"
SCRIPT = "fit_individual_kduq.py"

def main():
    if not os.path.exists(CONFIG_FILE):
        print(f"Error: {CONFIG_FILE} not found.")
        sys.exit(1)

    # Extract active state IDs
    state_ids = []
    with open(CONFIG_FILE, 'r') as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith('#'):
                parts = line.split('|')
                state_ids.append(parts[0].strip())

    print(f"Found {len(state_ids)} states to process.")

    results_summary = []

    for sid in state_ids:
        print(f"\n>>> Processing State {sid}...")
        try:
            # Run the individual fitting script
            subprocess.run(["python3.11", SCRIPT, sid], check=True)
            
            # Extract summary from the newly created .txt file
            txt_file = f"Results/fit_individual_state{sid}.txt"
            if os.path.exists(txt_file):
                with open(txt_file, 'r') as f:
                    lines = f.readlines()
                    avg_sf = "N/A"
                    std_dev = "N/A"
                    for line in lines:
                        if "Average SF:" in line:
                            avg_sf = line.split(":")[1].strip()
                        if "Std Dev:" in line:
                            std_dev = line.split(":")[1].strip()
                    results_summary.append(f"State {sid:2}: SF = {avg_sf:8} ± {std_dev}")
        except subprocess.CalledProcessError as e:
            print(f"Error processing state {sid}: {e}")
            results_summary.append(f"State {sid:2}: FAILED")

    # Save final summary
    print("\n" + "="*50)
    print("BATCH PROCESSING COMPLETE")
    print("="*50)
    summary_file = "Results/batch_fits_summary.txt"
    with open(summary_file, 'w') as f:
        f.write("Batch Fitting Summary (All States)\n")
        f.write("="*50 + "\n")
        for line in results_summary:
            print(line)
            f.write(line + "\n")
    
    print(f"\nSummary saved to {summary_file}")

if __name__ == "__main__":
    main()
