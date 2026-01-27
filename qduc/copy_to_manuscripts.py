#!/usr/bin/env python3.11
import os
import shutil
import re

# Configuration
CONFIG_FILE = "states.config"
RESULTS_DIR = "Results"
MANUSCRIPTS_BASE = "/Users/calemhoffman/Documents/GitHub/manuscripts/manuscripts/s36/dwba"

def get_mapping():
    mapping = {}
    with open(CONFIG_FILE, 'r') as f:
        for line in f:
            line = line.strip()
            if line.startswith('#') or not line:
                continue
            parts = [p.strip() for p in line.split('|')]
            state_id = int(parts[0])
            description = parts[10] if len(parts) > 10 else ""
            
            # Extract keV from description like 37S_2023_0f7/2 or 37S_gs_0f7/2
            keV = 0
            if "gs" in description:
                keV = 0
            else:
                match = re.search(r'37S_(\d+)_', description)
                if match:
                    keV = int(match.group(1))
                else:
                    # Fallback to Q-value calculation if pattern doesn't match
                    # Q = Q_gs - Ex -> Ex = Q_gs - Q
                    # Q_gs = 2.079
                    try:
                        q_val = float(parts[1])
                        ex_mev = 2.079 - q_val
                        keV = int(round(ex_mev * 1000))
                    except:
                        print(f"Warning: Could not determine keV for state {state_id}")
                        continue
            
            # Pattern: exXXX where XXX is keV // 10 padded
            ex_folder = f"ex{keV // 10:03d}"
            mapping[state_id] = ex_folder
            
    return mapping

def main():
    if not os.path.exists(RESULTS_DIR):
        print(f"Error: Results directory {RESULTS_DIR} not found.")
        return

    mapping = get_mapping()
    
    # Process states starting from 5
    for state_id in sorted(mapping.keys()):
        if state_id < 5:
            continue
            
        ex_folder = mapping[state_id]
        src_file = os.path.join(RESULTS_DIR, f"output_state{state_id}.dat")
        
        if not os.path.exists(src_file):
            print(f"Warning: Source file {src_file} not found. Skipping state {state_id}.")
            continue
            
        dest_dir = os.path.join(MANUSCRIPTS_BASE, ex_folder, "kduq")
        
        # Ensure destination directory and subdirectories exist
        # Also create dwuck and ptolemy as they are part of the standard structure
        if not os.path.exists(dest_dir):
            print(f"Creating directory: {dest_dir}")
            os.makedirs(dest_dir, exist_ok=True)
            
            # Create sibling ptolemy and dwuck directories for completeness
            os.makedirs(os.path.join(MANUSCRIPTS_BASE, ex_folder, "dwuck"), exist_ok=True)
            os.makedirs(os.path.join(MANUSCRIPTS_BASE, ex_folder, "ptolemy"), exist_ok=True)
        else:
            print(f"Directory exists: {dest_dir}")

        dest_file = os.path.join(dest_dir, f"output_state{state_id}.dat")
        print(f"Copying state {state_id} ({ex_folder}) -> {dest_file}")
        shutil.copy2(src_file, dest_file)

    print("\nCopy operation completed.")

if __name__ == "__main__":
    main()
