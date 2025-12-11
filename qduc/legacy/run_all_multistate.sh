#!/bin/bash

# QDUC Multi-State Master Script
# Reads states.config and runs calculations for each defined state

CONFIG_FILE="states.config"
INPUTS_BASE="Inputs"
OUTPUTS_BASE="Outputs"

if [ ! -f "$CONFIG_FILE" ]; then
    echo "Error: Configuration file '$CONFIG_FILE' not found!"
    exit 1
fi

echo "=========================================="
echo "QDUC Multi-State Calculation"
echo "=========================================="
echo "Reading configuration from: $CONFIG_FILE"
echo ""

# Read config file and process each state
while IFS='|' read -r state_id q_value l_transfer j_transfer nodes initial_spin final_spin target_mass target_z beam_energy description; do
    
    # Skip comments and empty lines
    [[ "$state_id" =~ ^#.*$ ]] && continue
    [[ -z "$state_id" ]] && continue
    
    # Trim whitespace from all variables
    state_id=$(echo "$state_id" | xargs)
    q_value=$(echo "$q_value" | xargs)
    l_transfer=$(echo "$l_transfer" | xargs)
    j_transfer=$(echo "$j_transfer" | xargs)
    nodes=$(echo "$nodes" | xargs)
    initial_spin=$(echo "$initial_spin" | xargs)
    final_spin=$(echo "$final_spin" | xargs)
    target_mass=$(echo "$target_mass" | xargs)
    target_z=$(echo "$target_z" | xargs)
    beam_energy=$(echo "$beam_energy" | xargs)
    description=$(echo "$description" | xargs)
    
    echo "=========================================="
    echo "Processing STATE $state_id: $description"
    echo "=========================================="
    echo "  Q-value:       $q_value MeV"
    echo "  Transfer (l,j): ($l_transfer, $j_transfer)"
    echo "  Nodes:         $nodes"
    echo "  Initial spin:  $initial_spin"
    echo "  Final spin:    $final_spin"
    echo "  Target:        ${target_mass} (Z=${target_z})"
    echo "  Beam energy:   $beam_energy MeV"
    echo ""
    
    # Create state-specific directories
    inputs_dir="${INPUTS_BASE}_state${state_id}"
    outputs_dir="${OUTPUTS_BASE}_state${state_id}"
    mkdir -p "$inputs_dir"
    mkdir -p "$outputs_dir"
    
    # Define input parameters based on config
    fixed_lines1=(
      "2"
      "0"
      "0"
      "$beam_energy"
      "$target_mass $target_z"
      "1"
      "1"
      "0 0 0"
      "$l_transfer $j_transfer"
      "$nodes"
      "2"
      "$q_value"
      "1"
      "$initial_spin"
      "1"
      "6"
      "1"
      "1"
      "$final_spin"
      "1"
      "7"
    )
    
    fixed_lines2=(
      "2"
      "1"
      "2"
      "1.25 0.65"
      "6"
      "0"
      "0"
    )
    
    # Loop through 416 KDUQ parameter sets
    echo "Running 416 KDUQ parameter sets for state $state_id..."
    for i in $(seq 1 416); do
        
        if [ $(($i % 50)) -eq 0 ]; then
            echo "  Progress: $i/416"
        fi
        
        # Pad iteration number to 3 digits (e.g., 001, 010, 416)
        padded_i=$(printf "%03d" $i)
        
        # Use short filename format: s{state_id}t{padded_i} (max 6 chars for s3t416)
        short_name="s${state_id}t${padded_i}"
        
        filename="$inputs_dir/input.$i"
        
        # Create input file
        rm -f input.front
        {
            echo "$short_name"
            echo "State $state_id: $description - KDUQ set $i"
            
            for line in "${fixed_lines1[@]}"; do
                echo "$line"
            done
            
            echo "$i"
            echo "5"
            echo "$i"
            
            for line in "${fixed_lines2[@]}"; do
                echo "$line"
            done
            
        } > $filename
        
        # Run FRONT_KDUQ
        ./FRONT_KDUQ < $filename > /dev/null 2>&1
        
        # Run TWOFNR
        tranfile="tran.$short_name"
        echo $tranfile > twofnr.input
        ./TWOFNR < twofnr.input > /dev/null 2>&1
        
        # Move outputs (using short_name pattern)
        mv *$short_name* $outputs_dir/ 2>/dev/null
        
    done
    
    echo "  ✓ State $state_id complete! Outputs in: $outputs_dir"
    echo ""
    
done < "$CONFIG_FILE"

echo "=========================================="
echo "All states completed!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "  1. Run: python3 process_multistate.py"
echo "  2. Run: python3 plot_multistate.py"
echo ""
