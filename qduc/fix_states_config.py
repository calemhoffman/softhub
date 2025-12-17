
import csv
import os

# Configuration
q_gs_ground = 2.079 # MeV
FIXED_Q_VALUE = q_gs_ground - 4.0

states_file = "states.config"
csv_file = "c2s_good_only.csv"

# 1. Truncate states.config to remove IDs >= 19
print("Truncating states.config...")
valid_lines = []
max_id = 0
with open(states_file, 'r') as f:
    for line in f:
        is_bad = False
        if line.strip() and not line.startswith('#'):
            parts = line.split('|')
            try:
                sid = int(parts[0])
                if sid >= 19:
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

# 2. Parse CSV and Append correctly
new_states = []
with open(csv_file, 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        energy = float(row['energy'])
        if energy > 4.08:
            # Extract parameters
            spin_float = float(row['spin'])
            spin_num = int(2 * spin_float)
            
            l_val = int(float(row['ell']))
            parity = "+" if l_val % 2 == 0 else "-"
            
            # Spin string for description e.g. 3/2-
            spin_str = f"{spin_num}/2{parity}"
            
            # Nodes logic
            nodes = 1 if l_val == 1 else 0
            
            j_trans = spin_float
            
            # Description format: 37S_{keV}_{n}{orbital}{j}
            # e.g. 37S_4147_1p1/2
            # Orbital map: 0->s, 1->p, 2->d, 3->f, 4->g
            orbitals = {0: 's', 1: 'p', 2: 'd', 3: 'f', 4: 'g'}
            orb_char = orbitals.get(l_val, '?')
            
            # Node prefix? Existing descriptions:
            # 37S_gs_0f7/2 (L=3 -> f, nodes=0 -> 0f)
            # 37S_644_1p3/2 (L=1 -> p, nodes=1 -> 1p)
            # 37S_1398_0d3/2 (L=2 -> d, nodes=0 -> 0d)
            # So {nodes}{orbital_char}{J_frac}
            # J_frac e.g. 3/2
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

print(f"Appending {len(new_states)} states...")

with open(states_file, 'a') as f:
    # Ensure newline start if needed (though writelines keeps newlines)
    # Check if last line has newline
    if valid_lines and not valid_lines[-1].endswith('\n'):
        f.write('\n')
        
    for s in new_states:
        max_id += 1
        q_val = FIXED_Q_VALUE
        
        # CORRECT FORMAT:
        # ID | Q | L | J | Nodes | InitSpin | FinalSpin | Mass | Z | Beam | Description
        # 1 | 2.079 | 3 | 3.5 | 0 | 0 | 3.5 | 36 | 16 | 8.0 | 37S_gs_0f7/2
        
        line = f"{max_id} | {q_val:.3f} | {s['l']} | {s['j']} | {s['nodes']} | 0 | {s['spin_float']} | 36 | 16 | 8.0 | {s['desc']}"
        f.write(line + "\n")
        print(f"Added: {line}")

print("Done.")
