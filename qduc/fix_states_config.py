
import csv
import os

# Configuration
q_gs_ground = 2.079 # MeV
# Strictly for Ex >= 4.0, Q = -1.921
FIXED_Q_VALUE = q_gs_ground - 4.0

states_file = "states.config"
csv_file = "c2s_good_only.csv"

# 1. Truncate states.config to remove IDs >= 18 
# (State 18 is at 4.072 MeV, which needs the fixed Q)
print("Truncating states.config to keep only States 1-17...")
valid_lines = []
max_id = 0
with open(states_file, 'r') as f:
    for line in f:
        is_bad = False
        stripped = line.strip()
        if stripped and not stripped.startswith('#'):
            parts = stripped.split('|')
            try:
                sid = int(parts[0])
                if sid >= 18:
                    is_bad = True
                else:
                    if sid > max_id:
                        max_id = sid
            except:
                pass
        
        if not is_bad:
            valid_lines.append(line)

with open(states_file, 'w') as f:
    f.writelines(valid_lines)

print(f"Truncated. Current max ID: {max_id}")

# 2. Parse CSV and Append correctly starting from energy matching State 18 (4.072)
new_states = []
with open(csv_file, 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        energy = float(row['energy'])
        # Last kept state was 17 (3.666). Next is 18 (4.005 is actually state 17, 4.072 is 18).
        # Let's check c2s_good_only.csv again.
        # 16: 3.605, 17: 3.666 (wait, c2s_good_only has 4.005 at index 18, 4.072 at 19)
        # states.config state 17 is 4.005. So we keep states 1-17.
        if energy > 4.05: 
            # Extract parameters
            spin_float = float(row['spin'])
            spin_num = int(2 * spin_float)
            
            l_val = int(float(row['ell']))
            
            # Nodes logic
            # L=1 -> 1, L!=1 -> 0 (based on previous states pattern for 36S(d,p))
            nodes = 1 if l_val == 1 else 0
            
            j_trans = spin_float
            
            # Description format: 37S_{keV}_{n}{orbital}{j}
            orbitals = {0: 's', 1: 'p', 2: 'd', 3: 'f', 4: 'g'}
            orb_char = orbitals.get(l_val, '?')
            j_frac = f"{spin_num}/2"
            
            desc_energy = int(energy * 1000) # keV
            description = f"37S_{desc_energy}_{nodes}{orb_char}{j_frac}"
            
            new_states.append({
                'energy': energy,
                'l': l_val,
                'j': j_trans,
                'nodes': nodes,
                'spin_float': spin_float,
                'desc': description
            })

print(f"Appending {len(new_states)} states from energy threshold 4.05 MeV...")

with open(states_file, 'a') as f:
    if valid_lines and not valid_lines[-1].endswith('\n'):
        f.write('\n')
        
    for s in new_states:
        max_id += 1
        # RULE: for all states with Ex >= 4.0 MeV, use Q = -1.921
        q_val = FIXED_Q_VALUE
        
        # FORMAT MUST BE 11 COLUMNS:
        # state_id | Q_value | l_transfer | j_transfer | nodes | initial_spin | final_spin | target_mass | target_Z | beam_energy | description
        
        line = f"{max_id} | {q_val:.3f} | {s['l']} | {s['j']} | {s['nodes']} | 0 | {s['spin_float']} | 36 | 16 | 8.0 | {s['desc']}"
        f.write(line + "\n")
        print(f"Added: {line}")

print("Done.")
