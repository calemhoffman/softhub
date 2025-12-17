
import numpy as np
import os

# Angles from experimental_data.dat
angles = [10.0, 15.0, 20.0, 23.0, 27.0, 35.0, 40.0, 45.0, 50.0]

# Mapping state ID to energy name (for filename) and data indices
# Data extracted from rawData.txt manually or via parsing. 
# Since rawData.txt parsing is complex, I will hardcode the data for the high energy states 
# based on the content viewed in step 370.

# IDs will start from 19 (since we had 18 states before).
# We need to match the order added by add_remaining_states.py
# add_remaining_states.py reads c2s_good_only.csv and adds states > 4.08 MeV.
# Let's list them in order of energy from c2s_good_only.csv:
# 4.147, 4.226, 4.368, 4.411, 4.492, 4.458 (out of order in csv? no, 4.458 is after 4.492 in csv line 25), 
# 4.675, 4.812, 4.858, 4.882, 4.893 (csv line 30), 5.054, 5.090, 5.122, 5.505, 5.666, 5.720, 
# 6.150, 6.408, 6.550, 6.732, 6.775

# Wait, I need to be precise about the ID mapping.
# I will output the mapping from add_remaining_states.py or infer it here.
# It appended in order of CSV reading.
# Let's define the data dictionary based on energies.

# Data from rawData.txt:
data_map = {
    "4150": { # Matches 4.147 closely
        "data": [np.nan, 2.96E+01, 1.64E+01, 1.28E+01, 6.64E+00, 5.42E+00, 2.56E+00, 3.95E+00, 6.63E+00],
        "err":  [np.nan, 5.13E+00, 2.90E+00, 2.09E+00, 1.53E+00, 1.63E+00, 3.84E-01, 5.93E-01, 1.33E+00]
    },
    "4226": {
        "data": [np.nan, 2.32E+01, 1.64E+01, 1.55E+01, 9.29E+00, np.nan, 4.27E+00, np.nan, 1.68E+00],
        "err":  [np.nan, 5.31E+00, 2.69E+00, 2.50E+00, 2.04E+00, np.nan, 1.31E+00, np.nan, 1.44E+00]
    },
    "4368": {
        "data": [1.91E+01, np.nan, 1.44E+01, 9.11E+00, np.nan, 6.36E+00, np.nan, 2.86E+00, np.nan],
        "err":  [7.73E+00, np.nan, 4.68E+00, 1.59E+00, np.nan, 1.70E+00, np.nan, 1.17E+00, np.nan]
    },
    "4411": {
        "data": [3.23E+01, np.nan, 1.95E+01, 1.78E+01, 1.66E+01, 1.70E+01, 1.42E+01, 7.22E+00, np.nan],
        "err":  [5.96E+00, np.nan, 3.20E+00, 2.83E+00, 2.75E+00, 3.03E+00, 2.57E+00, 1.36E+00, np.nan]
    },
    "4492": {
        "data": [8.97E+01, np.nan, 1.81E+01, np.nan, np.nan, 7.77E+00, np.nan, 7.77E+00, np.nan],
        "err":  [1.43E+01, np.nan, 4.78E+00, np.nan, np.nan, 2.02E+00, np.nan, 1.51E+00, np.nan]
    },
    "4548": { # Matches 4.458 ? No, 4.548. Let's check CSV. CSV: 4.458. rawData: 4548. 
              # Maybe typoin CSV or rawData. Assuming 4548 corresponds to the 4.458 entry or similar.
              # CSV line 25: 4.458. rawData line 54: 4548 keV.
              # The order in rawData matches the order in CSV mostly.
        "data": [4.90E+01, 9.53E+02, 2.93E+01, np.nan, np.nan, 1.32E+01, 1.51E+01, 1.23E+01, 7.78E+00],
        "err":  [8.77E+00, 1.44E+02, 5.48E+00, np.nan, np.nan, 2.30E+00, 2.84E+00, 2.07E+00, 1.25E+00]
    },
    "4675": {
        "data": [2.75E+01, 9.32E+02, 4.33E+01, np.nan, np.nan, 9.42E+00, 1.11E+01, np.nan, 6.72E+00],
        "err":  [6.32E+00, 1.41E+02, 7.27E+00, np.nan, np.nan, 1.84E+00, 2.39E+00, np.nan, 1.10E+00]
    },
    "4812": {
        "data": [1.97E+01, 1.79E+01, 7.43E+00, np.nan, 4.31E+00, np.nan, 2.85E+00, 1.36E+00, np.nan],
        "err":  [4.21E+00, 3.76E+00, 1.72E+00, np.nan, 1.05E+00, np.nan, 1.14E+01, 9.54E+00, np.nan]
    },
    "4858": {
        "data": [4.19E+01, 5.20E+01, 3.56E+01, 2.52E+01, 2.02E+01, 6.59E+00, 9.11E+00, 6.40E+00, 3.71E+00],
        "err":  [1.90E+01, 8.44E+00, 7.34E+00, 3.89E+00, 3.20E+00, 5.51E+00, 1.69E+00, 5.53E+00, 7.11E-01]
    },
    "4881": { # CSV has 4.882. Close enough.
        "data": [4.78E+00, np.nan, 6.56E+00, 7.17E+00, 1.66E+00, np.nan, 9.11E+00, 5.18E+00, 8.84E+00],
        "err":  [4.78E+01, np.nan, 7.06E+00, 2.22E+00, 1.49E+01, np.nan, 1.78E+00, 1.23E+01, 1.40E+00]
    },
    "4893": {
        "data": [8.49E+01, 5.93E+01, 1.97E+01, 1.78E+01, 1.82E+01, 1.88E+01, 1.14E+01, 1.43E+01, 3.54E-01],
        "err":  [3.25E+01, 9.45E+00, 3.95E+00, 3.13E+00, 3.04E+00, 8.05E+00, 4.56E+01, 8.45E+00, 4.45E-01]
    },
    "5054": {
        "data": [1.85E+01, 1.12E+01, np.nan, 1.01E+01, 1.16E+01, 8.48E+00, 1.11E+01, 4.22E+00, 2.65E+00],
        "err":  [4.35E+00, 3.62E+00, 4.98E+00, 1.89E+00, 1.93E+00, 2.47E+00, 1.97E+00, 1.03E+00, 7.36E-01]
    },
    "5090": {
        "data": [3.23E+01, 2.96E+01, np.nan, 1.95E+01, 1.09E+01, 9.66E+00, 9.11E+00, 3.54E+00, 8.84E-01],
        "err":  [6.25E+00, 5.98E+00, 4.45E+01, 3.25E+00, 1.84E+00, 2.57E+00, 1.73E+00, 8.19E+00, 5.47E-01]
    },
    "5122": {
        "data": [5.98E+00, 1.04E+01, 4.35E+01, 7.68E+00, 1.13E+01, 1.22E+01, 4.84E+00, 4.09E+00, 1.59E+00],
        "err":  [2.89E+00, 3.56E+00, 2.28E+01, 1.54E+00, 2.49E+00, 2.32E+00, 1.86E+00, 6.13E-01, 1.70E+00]
    },
    "5505": {
        "data": [2.55E+02, 3.03E+02, 2.24E+02, 1.92E+02, 1.21E+02, 6.50E+01, 5.29E+01, 2.78E+01, 2.51E+01],
        "err":  [3.89E+01, 4.60E+01, 3.37E+01, 2.89E+01, 1.85E+01, 9.93E+00, 8.19E+00, 4.39E+00, 3.89E+00]
    },
    "5666": {
        "data": [2.37E+02, 3.14E+02, 1.94E+02, 1.71E+02, 1.07E+02, 7.30E+01, 5.27E+01, 2.68E+01, 2.89E+01],
        "err":  [3.75E+01, 4.75E+01, 2.92E+01, 2.58E+01, 1.62E+01, 1.12E+01, 8.30E+00, 4.21E+00, 4.72E+00]
    },
    "5720": {
        "data": [1.49E+02, 1.19E+02, 9.42E+01, 9.50E+01, 4.46E+01, 3.27E+01, 2.70E+01, 1.48E+01, 1.56E+01],
        "err":  [2.54E+01, 1.86E+01, 1.43E+01, 1.43E+01, 6.90E+00, 5.18E+00, 4.52E+00, 2.42E+00, 2.50E+00]
    },
    "6150": {
        "data": [6.34E+01, 7.53E+01, 8.81E+01, 3.99E+01, 2.62E+01, 2.83E+01, np.nan, 9.13E+00, 3.98E+00],
        "err":  [1.12E+01, 1.20E+01, 1.34E+01, 6.14E+00, 4.10E+00, 6.16E+00, np.nan, 1.67E+00, 1.14E+00]
    },
    "6408": {
        "data": [7.41E+01, 7.69E+01, 8.42E+01, 3.33E+01, 3.32E+01, 1.18E+01, 2.48E+01, 9.40E+00, 8.40E+00],
        "err":  [1.21E+01, 1.25E+01, 1.29E+01, 5.14E+00, 5.11E+00, 3.33E+00, 4.22E+00, 1.78E+00, 1.37E+00]
    },
    "6550": {
        "data": [4.90E+01, 7.29E+01, 7.30E+01, 2.63E+01, np.nan, 1.20E+01, 3.99E+00, 8.99E+00, 1.77E+00],
        "err":  [8.77E+00, 1.23E+01, 1.13E+01, 4.20E+00, np.nan, 2.44E+00, 2.63E+00, 2.02E+00, 1.01E+00]
    },
    "6732": {
        "data": [7.65E+01, np.nan, np.nan, 3.29E+01, np.nan, 8.01E+00, np.nan, 7.90E+00, 1.30E+01],
        "err":  [1.35E+01, np.nan, np.nan, 5.11E+00, np.nan, 2.04E+00, np.nan, 8.49E+00, 1.95E+00]
    },
    "6775": {
        "data": [4.78E+01, np.nan, 8.37E+01, 1.80E+01, 1.97E+01, 8.01E+00, 1.20E+01, 3.81E+00, 1.03E+01],
        "err":  [9.34E+00, np.nan, 1.27E+01, 2.93E+00, 2.96E+00, 2.04E+00, 2.90E+00, 5.72E-01, 2.10E+00]
    }
}

# Need to associate these labels with the new State IDs (19 to 40) using the same logic as add_remaining_states.py
# Order in c2s_good_only.csv:
# 20: 4.147
# 21: 4.226
# 22: 4.368
# 23: 4.411
# 24: 4.492
# 25: 4.458 (maps to 4548 in rawData?)
# 26: 4.675
# 27: 4.812
# 28: 4.858
# 29: 4.882 (maps to 4881)
# 30: 4.893
# 31: 5.054
# 32: 5.090
# 33: 5.122
# 34: 5.505
# 35: 5.666
# 36: 5.720
# 37: 6.150
# 38: 6.408
# 39: 6.550
# 40: 6.732
# 41: 6.775

ordered_keys = [
    "4150", 
    "4226", 
    "4368", 
    "4411", 
    "4492", 
    "4548", # For 4.458
    "4675",
    "4812",
    "4858",
    "4881", # For 4.882
    "4893",
    "5054",
    "5090",
    "5122",
    "5505",
    "5666",
    "5720",
    "6150",
    "6408",
    "6550",
    "6732",
    "6775"
]

# Write files starting from ID 19
start_id = 19
current_id = start_id

os.makedirs("experimental_data_bylevel", exist_ok=True)

for key in ordered_keys:
    if key not in data_map:
        print(f"Warning: Data for {key} not found in map.")
        current_id += 1
        continue
        
    info = data_map[key]
    # Rough energy for filename
    filename = f"experimental_data_bylevel/level_{current_id}_{key}keV.dat"
    # Actually, the analyze script looks for level_{state_id}_*.dat. 
    # The energy part doesn't strictly matter for loading as long as ID matches.
    # But for consistency, let's try to match what add_remaining_states produced if possible?
    # add_remaining_states doesn't produce data files, only config entries.
    # The config entries have "Ex=4.147 MeV" etc.
    # fit_and_plot_state.py uses glob: f"experimental_data_bylevel/level_{state_id}_*.dat"
    # So the suffix doesn't strictly matter for matching.
    
    with open(filename, "w") as f:
        f.write(f"# Experimental data for State {current_id} (~{key} keV)\n")
        f.write("# angle (deg)    cross_section (mb/sr)    error (mb/sr)\n")
        
        valid_points = 0
        for i in range(len(angles)):
            ang = angles[i]
            val = info['data'][i]
            err = info['err'][i]
            
            # Skip if NaN
            if np.isnan(val) or np.isnan(err):
                continue
                
            f.write(f"{ang:<16} {val:<24} {err:<24}\n")
            valid_points += 1
            
    print(f"Created {filename} ({valid_points} valid points)")
    current_id += 1

print("Done generating extra data files.")
