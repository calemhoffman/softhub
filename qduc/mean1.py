#!/usr/bin/env python3
import numpy as np
import glob
import os

# === CONFIGURATION ===
pattern = "Outputs/21.s1t*"  # change if needed
output_file = "output.dat"
skip_rows = 0                 # set to 1 or 2 if your files have header lines
delimiter = None              # None = any whitespace; use ',' for csv, etc.

# Find and sort files
files = sorted(glob.glob(pattern))
if not files:
    raise FileNotFoundError(f"No files found matching pattern: {pattern}")

print(f"Found {len(files)} files:")
for f in files:
    print(f"  - {f}")

# Read the first file to get the reference angles
print(f"\nReading reference angles from {files[0]} ...")
data1 = np.loadtxt(files[0], usecols=(0,), skiprows=skip_rows, delimiter=delimiter)
angles = np.round(data1, decimals=6)  # round to avoid floating-point issues
n_points = len(angles)
print(f"Detected {n_points} angle points (0 to 180)")

# Stack all value columns (second column)
all_values = []

for i, fname in enumerate(files):
    print(f"Reading file {i+1}/{len(files)}: {fname}")
    try:
        col2 = np.loadtxt(fname, usecols=(1,), skiprows=skip_rows, delimiter=delimiter)
        col2_angles = np.loadtxt(fname, usecols=(0,), skiprows=skip_rows, delimiter=delimiter)
        col2_angles = np.round(col2_angles, decimals=6)

        # Critical check: do angles match the first file?
        if len(col2) != n_points:
            raise ValueError(f"Row count mismatch: expected {n_points}, got {len(col2)}")
        if not np.allclose(col2_angles, angles, rtol=1e-6, atol=1e-6):
            print(f"   Warning: Angle mismatch detected in {fname}! Trying to re-align...")
            # Try to regrid if angles are close but not identical
            col2_interp = np.interp(angles, col2_angles, col2,
                                   left=np.nan, right=np.nan)
            if np.any(np.isnan(col2_interp)):
                raise ValueError("Failed to interpolate due to NaNs")
            col2 = col2_interp
        all_values.append(col2)
    except Exception as e:
        raise RuntimeError(f"Failed to read {fname}: {e}")

# Convert to array: shape = (n_files, n_points)
data = np.array(all_values)

# Compute mean and standard deviation
mean = np.mean(data, axis=0)
std  = np.std(data, axis=0)        # population std dev
# use ddof=1 below for sample standard deviation if you prefer:
# std = np.std(data, axis=0, ddof=1)

# Build output columns
plus1 = mean + std
plus2 = mean + 2*std
minus1 = mean - std
minus2 = mean - 2*std

# Save result
header = ("angle        average        +1sigma        +2sigma        "
          "-1sigma        -2sigma")

np.savetxt(output_file,
           np.column_stack([angles, mean, plus1, plus2, minus1, minus2]),
           fmt="%.10f",
           header=header,
           comments='# ')

print("\nSuccess! Output written to:", output_file)
print(f"   Angles : {angles[0]:.2f} to {angles[-1]:.2f} ({n_points} points)")
print(f"   Mean range   : {mean.min():.6f} to {mean.max():.6f}")
print(f"   Sigma range  : {std.min():.6f} to {std.max():.6f}")