#!/usr/bin/env python3
"""
Process multi-state QDUC results
Runs mean1.py logic for each state separately
"""
import numpy as np
import glob
import os
import sys

CONFIG_FILE = "states.config"
OUTPUTS_BASE = "Outputs"
RESULTS_DIR = "Results"

# Create results directory
os.makedirs(RESULTS_DIR, exist_ok=True)

print("=" * 70)
print("Processing Multi-State QDUC Results")
print("=" * 70)
print("")

# Read config file to get state IDs
states = []
with open(CONFIG_FILE, 'r') as f:
    for line in f:
        line = line.strip()
        if line.startswith('#') or not line:
            continue
        parts = [p.strip() for p in line.split('|')]
        state_id = parts[0]
        description = parts[9] if len(parts) > 9 else f"state_{state_id}"
        states.append((state_id, description))

print(f"Found {len(states)} active states in config file")
print("")

# Process each state
for state_id, description in states:
    print(f"Processing State {state_id}: {description}")
    print("-" * 70)
    
    outputs_dir = f"{OUTPUTS_BASE}_state{state_id}"
    output_file = f"{RESULTS_DIR}/output_state{state_id}.dat"
    
    if not os.path.exists(outputs_dir):
        print(f"  WARNING: Output directory not found: {outputs_dir}")
        print(f"  Skipping state {state_id}")
        print("")
        continue
    
    # Find all output files for this state
    pattern = f"{outputs_dir}/21.s{state_id}t*"
    files = sorted(glob.glob(pattern))
    
    if not files:
        print(f"  ERROR: No output files found matching: {pattern}")
        print(f"  Skipping state {state_id}")
        print("")
        continue
    
    print(f"  Found {len(files)} output files")
    
    # Read the first file to get angles
    data1 = np.loadtxt(files[0], usecols=(0,))
    angles = np.round(data1, decimals=6)
    n_points = len(angles)
    print(f"  Detected {n_points} angle points")
    
    # Stack all cross section values
    all_values = []
    for i, fname in enumerate(files):
        try:
            col2 = np.loadtxt(fname, usecols=(1,))
            if len(col2) != n_points:
                print(f"  WARNING: Row count mismatch in {fname}")
                continue
            all_values.append(col2)
        except Exception as e:
            print(f"  ERROR reading {fname}: {e}")
            continue
    
    if not all_values:
        print(f"  ERROR: No valid data read for state {state_id}")
        print("")
        continue
    
    # Convert to array and compute statistics
    data = np.array(all_values)
    mean = np.mean(data, axis=0)
    std = np.std(data, axis=0)
    
    plus1 = mean + std
    plus2 = mean + 2*std
    minus1 = mean - std
    minus2 = mean - 2*std
    
    # Save results
    header = ("angle        average        +1sigma        +2sigma        "
              "-1sigma        -2sigma")
    
    np.savetxt(output_file,
               np.column_stack([angles, mean, plus1, plus2, minus1, minus2]),
               fmt="%.10f",
               header=header,
               comments='# ')
    
    print(f"  ✓ Results saved to: {output_file}")
    print(f"    Mean range: {mean.min():.6f} to {mean.max():.6f} mb/sr")
    print(f"    Sigma range: {std.min():.6f} to {std.max():.6f} mb/sr")
    print("")

print("=" * 70)
print("Processing complete!")
print(f"All results saved to: {RESULTS_DIR}/")
print("")
print("Next step: Run 'python3 plot_multistate.py' to visualize all states")
print("=" * 70)
