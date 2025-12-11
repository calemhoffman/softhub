#!/usr/bin/env python3
"""
Extract experimental data from rawData.txt into structured .dat files
"""
import numpy as np
import re
import os

# Configuration
RAW_DATA_FILE = "rawData.txt"
OUTPUT_DIR = "experimental_data_bylevel"
COMBINED_FILE = "experimental_data_combined.dat"

# Angles from rawData.txt context (based on list indices)
# Standard angles seem to be: 10, 15, 20, 23, 27, 35, 40, 45, 50
ANGLES = [10.0, 15.0, 20.0, 23.0, 27.0, 35.0, 40.0, 45.0, 50.0]

# Level mapping (Index in file -> State ID & Energy)
LEVELS = [
    (0, 1, 0),      # Index 0 -> State 1 (0 keV)
    (1, 2, 644),    # Index 1 -> State 2 (644 keV)
    (2, 3, 1398),   # Index 2 -> State 3 (1398 keV)
    (3, 4, 1991),   # Index 3 -> State 4 (1991 keV)
    (4, 5, 2023),   # Index 4 -> State 5 (2023 keV)
    (5, 6, 2514),   # Index 5 -> State 6 (2514 keV)
    (6, 7, 2637),   # Index 6 -> State 7 (2637 keV)
    (7, 8, 2776),   # Index 7 -> State 8 (2776 keV)
    (8, 9, 3120)    # Index 8 -> State 9 (3120 keV)
]

def parse_scientific_list(text):
    """Parse a list of scientific notation numbers from text string"""
    # Remove brackets and split
    clean_text = text.strip().strip('[').strip('],')
    # Use regex to find numbers like 1.42E+02, np.nan, etc.
    values = []
    for item in clean_text.split(','):
        item = item.strip()
        if 'np.nan' in item:
            values.append(np.nan)
        else:
            try:
                values.append(float(item))
            except ValueError:
                print(f"Warning: Could not parse '{item}'")
                values.append(np.nan)
    return values

def extract_data():
    print(f"Reading {RAW_DATA_FILE}...")
    with open(RAW_DATA_FILE, 'r') as f:
        content = f.read()

    # Extract y_data_list chunks
    # We look for "y_data_list = [" and extract the array lists
    # This is a bit manual because the file format is somewhat unstructured python code
    
    # Split content into sections (AD, Error)
    sections = content.split("~~~~~")
    
    ad_section = ""
    error_section = ""
    
    for i, section in enumerate(sections):
        if "AD:" in section:
            ad_section = sections[i+1] # The section after the header
        if "ERROR:" in section:
            error_section = sections[i+1]

    if not ad_section or not error_section:
        print("ERROR: Could not find AD or ERROR sections")
        return

    # Extract rows from sections
    def extract_rows(section_text):
        rows = []
        for line in section_text.split('\n'):
            line = line.strip()
            if line.startswith('[') and ('E+' in line or 'np.nan' in line):
                # Check if it looks like a data row
                # Remove comments (#...)
                if '#' in line:
                    line = line.split('#')[0]
                rows.append(parse_scientific_list(line))
        return rows

    cross_sections = extract_rows(ad_section)
    errors = extract_rows(error_section)
    
    print(f"Found {len(cross_sections)} cross section rows and {len(errors)} error rows")
    
    # Process the first 9 levels
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    combined_data = []
    
    print("\nExtracting levels:")
    print("------------------")
    
    for idx, state_id, energy in LEVELS:
        if idx >= len(cross_sections) or idx >= len(errors):
            print(f"Error: Index {idx} out of range")
            continue
            
        sigma = cross_sections[idx]
        err = errors[idx]
        
        # Verify lengths
        if len(sigma) != len(ANGLES) or len(err) != len(ANGLES):
            print(f"Warning: State {state_id} ({energy} keV) has mismatching angles length")
            
        # Write individual file
        outfile = f"{OUTPUT_DIR}/level_{state_id}_{energy}keV.dat"
        with open(outfile, 'w') as f:
            f.write(f"# State {state_id}: 37S level at {energy} keV\n")
            f.write(f"# Angle(deg)  Cross_Section(mb/sr)  Error(mb/sr)\n")
            
            valid_points = 0
            for i, angle in enumerate(ANGLES):
                if i < len(sigma) and i < len(err):
                    s = sigma[i]
                    e = err[i]
                    
                    if not np.isnan(s) and not np.isnan(e):
                        f.write(f"{angle:5.1f}  {s:12.4E}  {e:12.4E}\n")
                        combined_data.append([state_id, energy, angle, s, e])
                        valid_points += 1
                        
        print(f"State {state_id} ({energy} keV): {valid_points} valid points -> {outfile}")

    # Write combined file
    with open(COMBINED_FILE, 'w') as f:
        f.write("# Combined Experimental Data for 36S(d,p)37S\n")
        f.write("# StateID  Energy(keV)  Angle(deg)  Cross_Section(mb/sr)  Error(mb/sr)\n")
        for row in combined_data:
            f.write(f"{int(row[0]):3d}  {int(row[1]):6d}  {row[2]:5.1f}  {row[3]:12.4E}  {row[4]:12.4E}\n")
            
    print(f"\nCombined data saved to: {COMBINED_FILE}")
    print(f"Total valid data points: {len(combined_data)}")

if __name__ == "__main__":
    extract_data()
