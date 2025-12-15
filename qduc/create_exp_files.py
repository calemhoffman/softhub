
import numpy as np
import os

# Angles from experimental_data.dat
angles = [10.0, 15.0, 20.0, 23.0, 27.0, 35.0, 40.0, 45.0, 50.0]

# Mapping state ID to energy name (for filename) and data indices
# Data extracted from rawData.txt
states_data = {
    10: {
        "energy": "3180",
        "data": [2.51E+01, 2.23E+01, 1.71E+01, np.nan, 1.03E+01, 8.48E+00, 8.82E+00, np.nan, 6.01E+00],
        "err":  [7.07E+00, 4.26E+00, 2.87E+00, np.nan, 2.04E+00, 1.73E+00, 1.75E+00, np.nan, 1.09E+00]
    },
    11: {
        "energy": "3262",
        "data": [2.21E+02, 2.06E+02, 9.95E+01, 4.42E+01, 3.73E+01, 3.25E+01, 3.42E+01, 2.85E+01, 1.62E+01],
        "err":  [3.37E+01, 3.12E+01, 1.50E+01, 6.70E+00, 5.72E+00, 5.08E+00, 5.40E+00, 4.35E+00, 2.51E+00]
    },
    12: {
        "energy": "3355",
        "data": [4.78E+01, 3.76E+01, 4.13E+01, 2.81E+01, 1.64E+01, 1.20E+01, 8.54E+00, 7.08E+00, 1.12E+01],
        "err":  [1.10E+01, 6.49E+00, 6.29E+00, 4.29E+00, 2.66E+00, 2.15E+00, 1.28E+00, 1.34E+00, 1.83E+00]
    },
    13: {
        "energy": "3442",
        "data": [5.02E+01, np.nan, 3.00E+01, 2.14E+01, 3.28E+01, 1.77E+01, 7.40E+00, 5.45E+00, 4.86E+00],
        "err":  [9.62E+00, 6.03E+01, 4.75E+00, 3.31E+00, 5.06E+00, 2.90E+00, 1.92E+00, 1.06E+00, 9.02E-01]
    },
    14: {
        "energy": "3493", # Using 3493 to match states.config
        "data": [1.03E+02, 8.41E+01, 4.37E+01, 2.24E+01, 1.26E+01, 1.01E+01, 1.28E+01, 1.02E+01, 8.84E+00],
        "err":  [1.65E+01, 1.32E+01, 6.91E+00, 3.49E+00, 2.22E+00, 1.92E+00, 2.48E+00, 1.74E+00, 1.46E+00]
    },
    15: {
        "energy": "3605",
        "data": [1.55E+01, 1.52E+01, 1.18E+01, 9.83E+00, np.nan, np.nan, np.nan, 3.13E+00, 2.74E+00],
        "err":  [1.22E+01, 8.33E+00, 2.81E+00, 1.79E+00, 1.54E+01, 3.49E+00, 3.01E+00, 1.44E+00, 9.75E-01]
    },
    16: {
        "energy": "3666",
        "data": [2.87E+01, 2.32E+01, 1.81E+01, 7.78E+00, 9.79E+00, 8.01E+00, 7.97E+00, 3.00E+00, 1.77E+00],
        "err":  [1.27E+01, 8.73E+00, 3.49E+00, 1.55E+00, 2.22E+00, 2.64E+00, 3.09E+00, 1.43E+00, 9.23E-01]
    },
    17: {
        "energy": "4005",
        "data": [np.nan, 3.60E+01, 2.45E+01, 1.22E+01, 9.29E+00, 8.95E+00, 8.25E+00, 8.17E+00, 3.98E+00],
        "err":  [np.nan, 6.05E+00, 4.39E+00, 2.00E+00, 2.30E+00, 1.79E+00, 1.68E+00, 1.64E+00, 9.26E-01]
    },
    18: {
        "energy": "4072",
        "data": [np.nan, 2.55E+01, 1.03E+01, 4.61E+00, 4.65E+00, 2.36E+00, 2.85E+00, 1.91E+00, 4.69E+00],
        "err":  [np.nan, 8.87E+00, 2.67E+00, 1.24E+00, 1.80E+00, 2.38E+00, 2.88E+00, 1.39E+00, 3.61E+00]
    }
}

os.makedirs("experimental_data_bylevel", exist_ok=True)

print("Generating experimental data files...")

for sid, info in states_data.items():
    filename = f"experimental_data_bylevel/level_{sid}_{info['energy']}keV.dat"
    
    with open(filename, "w") as f:
        f.write(f"# Experimental data for State {sid} ({info['energy']} keV)\n")
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

print("Done.")
