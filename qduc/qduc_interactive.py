#!/usr/bin/env python3
"""
QDUC Interactive Setup - Standalone Version
No web server needed - runs directly in terminal with a nice interface
"""

import os
import subprocess
import sys

def clear_screen():
    os.system('clear' if os.name != 'nt' else 'cls')

def print_header():
    print("╔" + "═" * 78 + "╗")
    print("║" + " " * 20 + "🔬 QDUC Nuclear Reaction Calculator" + " " * 23 + "║")
    print("║" + " " * 15 + "Interactive Parameter Setup & Execution" + " " * 24 + "║")
    print("╚" + "═" * 78 + "╝")
    print()

def print_section(title):
    print("\n" + "─" * 80)
    print(f"  {title}")
    print("─" * 80)

def get_input(prompt, default=None, help_text=None):
    """Get user input with optional default and help text"""
    if help_text:
        print(f"  ℹ️  {help_text}")
    if default:
        user_input = input(f"  {prompt} [{default}]: ").strip()
        return user_input if user_input else str(default)
    else:
        return input(f"  {prompt}: ").strip()

def select_orbital():
    """Interactive orbital selection"""
    orbitals = [
        ("s₁/₂", "0", "0.5"),
        ("p₁/₂", "1", "0.5"),
        ("p₃/₂", "1", "1.5"),
        ("d₃/₂", "2", "1.5"),
        ("d₅/₂", "2", "2.5"),
        ("f₅/₂", "3", "2.5"),
        ("f₇/₂", "3", "3.5"),
        ("g₇/₂", "4", "3.5"),
        ("g₉/₂", "4", "4.5"),
        ("h₉/₂", "5", "4.5"),
        ("h₁₁/₂", "5", "5.5"),
        ("i₁₁/₂", "6", "5.5"),
        ("i₁₃/₂", "6", "6.5"),
    ]
    
    print("\n  Available orbitals:")
    for i, (name, l, j) in enumerate(orbitals, 1):
        print(f"    [{i:2d}] {name:6s} (l={l}, j={j})")
    
    while True:
        choice = input("\n  Select orbital [1-13]: ").strip()
        try:
            idx = int(choice) - 1
            if 0 <= idx < len(orbitals):
                return orbitals[idx]
        except ValueError:
            pass
        print("  ❌ Invalid choice. Please enter a number between 1 and 13.")

def select_target():
    """Quick target selection"""
    targets = [
        ("Custom", None, None),
        ("³⁶S (Sulfur)", "36", "16"),
        ("⁴⁰Ca (Calcium)", "40", "20"),
        ("¹³²Sn (Tin)", "132", "50"),
        ("²⁰⁸Pb (Lead)", "208", "82"),
    ]
    
    print("\n  Quick target selection:")
    for i, (name, a, z) in enumerate(targets):
        print(f"    [{i}] {name}")
    
    while True:
        choice = input("\n  Select target [0-4]: ").strip()
        try:
            idx = int(choice)
            if 0 <= idx < len(targets):
                return targets[idx]
        except ValueError:
            pass
        print("  ❌ Invalid choice. Please enter a number between 0 and 4.")

def generate_script(params):
    """Generate the run script with user parameters"""
    script = f'''#!/bin/bash

# Auto-generated QDUC calculation script
# Reaction: {params['name']}

fixed_lines1=(
  "2"
  "0"
  "0"
  "{params['energy']}"
  "{params['a']} {params['z']}"
  "1"
  "1"
  "0 0 0"
  "{params['l']} {params['j']}"
  "{params['nodes']}"
  "2"
  "{params['q']}"
  "1"
  "{params['target_spin']}"
  "1"
  "6"
  "4"
  "1"
  "{params['j']}"
  "1"
  "7"
)

fixed_lines2=(
  "5"
  "1"
  "3"
  "1"
  "2"
  "1.25 0.65"
  "6"
  "0"
  "1.1"
  "0.65"
)

inputs_dir="Inputs"
outputs_dir="Outputs"
mkdir -p "$inputs_dir"
mkdir -p "$outputs_dir"

echo "Starting calculation for {params['name']}..."
echo "Target: A={params['a']}, Z={params['z']}"
echo "Beam energy: {params['energy']} MeV"
echo "Orbital: l={params['l']}, j={params['j']}"
echo "Q-value: {params['q']} MeV"
echo ""

for i in $(seq 1 416); do
  filename="$inputs_dir/input.$i"
  rm -f input.front
  {{
    echo "{params['name']}$i"
    echo "Running with KDUQ parameter set $i"
    for line in "${{fixed_lines1[@]}}"; do
      echo "$line"
    done
    echo "$i"
    echo "5"
    echo "$i"
    for line in "${{fixed_lines2[@]}}"; do
      echo "$line"
    done
  }} > $filename

  ./FRONT_KDUQ < $filename > /dev/null 2>&1
  
  tranfile="tran.{params['name']}$i"
  echo $tranfile > twofnr.input
  ./TWOFNR < twofnr.input > /dev/null 2>&1
  
  mv *.{params['name']}* $outputs_dir/ 2>/dev/null

  if [ $((i % 50)) -eq 0 ]; then
    echo "Progress: $i/416 iterations complete"
  fi
done

echo ""
echo "✓ All 416 iterations complete!"
echo "✓ Outputs saved to: $outputs_dir/"
'''
    return script

def main():
    clear_screen()
    print_header()
    
    print("  This interactive tool will guide you through setting up and running")
    print("  a QDUC nuclear reaction calculation with 416 KDUQ parameter sets.")
    print()
    
    # Collect parameters
    params = {}
    
    # Section 1: Reaction Setup
    print_section("📋 REACTION SETUP")
    
    params['name'] = get_input(
        "Reaction name (for file naming)",
        default="36S_dp",
        help_text="Use descriptive name like '36S_dp_d32' or '132Sn_dp'"
    )
    
    params['energy'] = get_input(
        "Deuteron beam energy (MeV/nucleon)",
        default="10.0",
        help_text="Laboratory energy of the deuteron beam"
    )
    
    # Target selection
    target_name, a, z = select_target()
    
    if a is None:  # Custom target
        params['a'] = get_input("Target mass number (A)", help_text="Total nucleons in target nucleus")
        params['z'] = get_input("Target atomic number (Z)", help_text="Number of protons")
    else:
        params['a'] = a
        params['z'] = z
        print(f"  ✓ Selected: {target_name}")
    
    # Section 2: Transferred Particle
    print_section("⚛️  TRANSFERRED NEUTRON STATE")
    
    orbital_name, l, j = select_orbital()
    params['l'] = l
    params['j'] = j
    print(f"  ✓ Selected: {orbital_name} (l={l}, j={j})")
    
    params['nodes'] = get_input(
        "Number of radial nodes",
        default="0",
        help_text="Usually 0 for lowest state (1s, 1p, 1d, etc.)"
    )
    
    params['q'] = get_input(
        "Reaction Q-value (MeV)",
        default="4.0",
        help_text="Q = Sₙ(final nucleus) - 2.224 MeV"
    )
    
    params['target_spin'] = get_input(
        "Target nucleus spin",
        default="0",
        help_text="Ground state spin of target (often 0 for even-even nuclei)"
    )
    
    # Summary
    print_section("📊 CALCULATION SUMMARY")
    print(f"  Reaction:       {params['name']}")
    print(f"  Target:         A={params['a']}, Z={params['z']}")
    print(f"  Beam energy:    {params['energy']} MeV/nucleon")
    print(f"  Orbital:        {orbital_name} (l={params['l']}, j={params['j']})")
    print(f"  Nodes:          {params['nodes']}")
    print(f"  Q-value:        {params['q']} MeV")
    print(f"  Target spin:    {params['target_spin']}")
    print(f"  Final spin:     {params['j']}")
    print()
    
    # Confirmation
    confirm = input("  Proceed with calculation? [Y/n]: ").strip().lower()
    if confirm and confirm != 'y':
        print("\n  ❌ Calculation cancelled.")
        return
    
    # Generate and save script
    print("\n" + "─" * 80)
    print("  🔧 Generating calculation script...")
    
    script_content = generate_script(params)
    script_path = 'run_calculation_custom.sh'
    
    with open(script_path, 'w') as f:
        f.write(script_content)
    
    os.chmod(script_path, 0o755)
    print(f"  ✓ Script saved to: {script_path}")
    
    # Clean old data
    print("  🧹 Cleaning old outputs...")
    subprocess.run(['rm', '-rf', 'Inputs', 'Outputs'], check=False)
    subprocess.run(['mkdir', '-p', 'Inputs', 'Outputs'], check=True)
    
    # Execute
    print("\n" + "─" * 80)
    print("  🚀 Starting calculation (416 iterations)...")
    print("  This will take several minutes. Progress will be shown below.")
    print("─" * 80)
    print()
    
    try:
        subprocess.run(['bash', script_path], check=True)
        
        print("\n" + "═" * 80)
        print("  ✅ CALCULATION COMPLETE!")
        print("═" * 80)
        print()
        print("  Next steps:")
        print("    1. Analyze results:  python3.11 mean1.py")
        print("    2. Generate plot:    python3.11 plot_results.py")
        print()
        
    except KeyboardInterrupt:
        print("\n\n  ⚠️  Calculation interrupted by user.")
        print("  Partial results may be in Outputs/ directory.")
    except Exception as e:
        print(f"\n  ❌ Error during calculation: {e}")
        return 1
    
    return 0

if __name__ == '__main__':
    sys.exit(main())
