
import os

config_file = "states.config"
count = 0
print(f"Checking {config_file}...")
with open(config_file, 'r') as f:
    for line in f:
        line = line.strip()
        if line and not line.startswith('#'):
            try:
                # ACTIVE STATE LINE
                parts = line.split('|')
                sid = int(parts[0])
                desc = parts[-1].strip() if len(parts) > 1 else "?"
                # Print only first and last few to keep output clean, plus total count
                if count < 5 or count >= 35: 
                    print(f"  Found State {sid}: {desc}")
                elif count == 5:
                    print("  ... (hiding intermediate states) ...")
                count += 1
            except ValueError:
                pass

print(f"Total active states found: {count}")
if count >= 40:
    print("SUCCESS: All 40 states are present.")
else:
    print(f"WARNING: Expected 40 states, found {count}.")
