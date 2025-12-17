
import csv
import os

# Configuration
q_gs_ground = 2.079 # MeV
# User rule: for Ex > 4.0 MeV, use Q-value as if Ex = 4.0 MeV (approximate, or strictly cap Q)
# User said: "for all states above Ex = 4 MeV, use a q-value that is equal to Ex = 4 MeV"
# This implies Q = Q_gs - 4.0 = -1.921 MeV for all these states.
FIXED_Q_VALUE = q_gs_ground - 4.0

states_file = "states.config"
csv_file = "c2s_good_only.csv"
raw_data_file = "rawData.txt"

# 1. Read existing max state ID
max_id = 0
with open(states_file, 'r') as f:
    for line in f:
        if line.strip() and not line.startswith('#'):
            parts = line.split('|')
            try:
                sid = int(parts[0])
                if sid > max_id:
                    max_id = sid
            except:
                pass

print(f"Current max state ID: {max_id}")

# 2. Parse CSV for new states (Energy > 4.072)
# The last state added was 4.072 MeV (State 18).
# We need to find states in CSV with energy > 4.072
new_states = []
with open(csv_file, 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        energy = float(row['energy'])
        if energy > 4.08: # Slightly higher to avoid float comparison issues with 4.072
            # Extract parameters
            spin_float = float(row['spin'])
            # Convert spin (e.g. 0.5 -> 1/2)
            spin_num = int(2 * spin_float)
            spin_str = f"{spin_num}/2"
            
            # Parity? c2s_good_only doesn't explicitly have parity column, 
            # but usually L determines it relative to target.
            # 36S(d,p)37S: 36S is 0+. 
            # p parity = (-1)^L
            # So final parity = (-1)^L
            l_val = int(float(row['ell']))
            parity = "+" if l_val % 2 == 0 else "-"
            final_spin = f"{spin_str}{parity}"
            
            # Nodes: L=1 -> 1, L!=1 -> 0 (based on previous states pattern)
            # Actually, looking at states.config:
            # L=3 -> nodes=0, L=1 -> nodes=1, L=2 -> nodes=0 
            # It seems generally n=0 for most, but L=1 is n=1 (2p states? 37S ground is 7/2- (1f7/2)).
            # This node logic might be shell model dependent. 
            # 1f7/2 (gs) node=0
            # 2p3/2 (0.644) node=1
            # 1d3/2 (1.398) node=0
            # 2p? 
            # Let's infer nodes from similar existing states or just default to 0 if L!=1, 1 if L=1?
            # State 4 (1.991) L=1, nodes=1.
            # State 7 (2.637) L=1, nodes=1.
            # State 11 (3.262) L=1, nodes=1.
            # It seems robust for this nucleus/reaction that L=1 uses nodes=1.
            nodes = 1 if l_val == 1 else 0
            # State 9 (3.120) L=4, nodes=0
            
            j_trans = spin_float # J transfer usually matches final spin for 0+ target
            
            new_states.append({
                'energy': energy,
                'l': l_val,
                'j': j_trans,
                'spin': final_spin,
                'nodes': nodes
            })

print(f"Found {len(new_states)} new states to add.")

# 3. Append to states.config
with open(states_file, 'a') as f:
    for s in new_states:
        max_id += 1
        # Calculate Q
        # Rule: if Ex > 4.0, use Q for Ex=4.0
        q_val = FIXED_Q_VALUE
        
        # Format: ID | Q | Nodes | L | J | InitialSpin | TargetMass | TargetZ | BeamEnergy | FinalSpin | MaxRadius | MatchingRadius | Description
        # Using standard params for the rest
        # 36. | 16. | 8.0 | 25.0 | 4.0
        line = f"{max_id} | {q_val:.4f} | {s['nodes']} | {s['l']} | {s['j']} | 0. | 36. | 16. | 8.0 | {s['spin']} | 25.0 | 4.0 | Ex={s['energy']:.3f} MeV"
        f.write(line + "\n")
        print(f"Added State {max_id}: {line}")

# 4. Generate experimental data files for these new states
# Need to match energies in rawData.txt
# rawData.txt defines lists like: ["4881 keV", ...]
# And data lists y_data_list. 
# We need to map our new states (by energy) to the rawData structure.

# Let's create a separate script or just do it here?
# Doing it here requires parsing rawData.txt robustly.
# Given the complexity of rawData.txt (multiple lists), let's use the pattern from step 322 but expanded.
