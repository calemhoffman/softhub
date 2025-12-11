#!/bin/bash

# Simpler parallel approach using background jobs and wait
# More reliable than xargs for Fortran programs that write to current directory

CONFIG_FILE="states.config"
NUM_CORES=8
MAX_JOBS=8  # Maximum parallel jobs

echo "=========================================="
echo "QDUC Multi-State PARALLEL Calculation"
echo "=========================================="
echo "Max parallel jobs: $MAX_JOBS"
echo "Reading configuration from: $CONFIG_FILE"
echo ""

# Read states
states=()
while IFS='|' read -r state_id Q_value l_transfer j_transfer nodes initial_spin final_spin target_mass target_Z beam_energy description; do
    [[ "$state_id" =~ ^[[:space:]]*#.*$ ]] && continue
    [[ -z "$state_id" ]] && continue
    
    state_id=$(echo "$state_id" | xargs)
    Q_value=$(echo "$Q_value" | xargs)
    l_transfer=$(echo "$l_transfer" | xargs)
    j_transfer=$(echo "$j_transfer" | xargs)
    nodes=$(echo "$nodes" | xargs)
    initial_spin=$(echo "$initial_spin" | xargs)
    final_spin=$(echo "$final_spin" | xargs)
    target_mass=$(echo "$target_mass" | xargs)
    target_Z=$(echo "$target_Z" | xargs)
    beam_energy=$(echo "$beam_energy" | xargs)
    description=$(echo "$description" | xargs)
    
    states+=("$state_id|$Q_value|$l_transfer|$j_transfer|$nodes|$initial_spin|$final_spin|$target_mass|$target_Z|$beam_energy|$description")
done < "$CONFIG_FILE"

echo "Found ${#states[@]} states to process"
echo ""

# Process each state
for state_data in "${states[@]}"; do
    
    IFS='|' read -r state_id Q_value l_transfer j_transfer nodes initial_spin final_spin target_mass target_Z beam_energy description <<< "$state_data"
    
    echo "=========================================="
    echo "Processing STATE $state_id: $description"
    echo "=========================================="
    echo "  Q-value:       $Q_value MeV"
    echo "  Transfer (l,j): ($l_transfer, $j_transfer)"
    echo "  Nodes:         $nodes"
    echo "  Initial spin:  $initial_spin"
    echo "  Final spin:    $final_spin"
    echo "  Target:        $target_mass (Z=$target_Z)"
    echo "  Beam energy:   $beam_energy MeV"
    echo ""
    
    # Create directories and clean old outputs to prevent false positives
    inputs_dir="Inputs_state${state_id}"
    outputs_dir="Outputs_state${state_id}"
    workdir="Work_state${state_id}"
    rm -rf "$inputs_dir" "$outputs_dir" "$workdir"
    mkdir -p "$inputs_dir" "$outputs_dir" "$workdir"
    
    # Fixed parameters matching run_all.sh EXACTLY
    # Note: Some values (like 1.25 0.65) are hardcoded in run_all.sh
    fixed_lines1=(
        "2"                         # Reaction type (d,p)
        "0"                         # Entrance channel calc
        "0"                         # Exit channel calc
        "$beam_energy"              # Beam energy
        "$target_mass $target_Z"    # Target A and Z
        "1"                         # Integration ranges
        "1"                         # Partial waves
        "0 0 0"                     # Angles
        "$l_transfer $j_transfer"   # Transfer L and J
        "$nodes"                    # Nodes
        "2"                         # Specify Q-value
        "$Q_value"                  # Q-value
        "1"                         # ?
        "$initial_spin"             # Initial spin
        "1"                         # ?
        "6"                         # ?
        "1"                         # ?
        "1"                         # Non-locality incident (Line 20)
        "$final_spin"               # Target spin outgoing (Line 21)
        "1"                         # Outgoing potential type (Line 22)
        "7"                         # Final potential choices (7=KDUQ, Line 23)
    )
    
    fixed_lines2=(
        "2"             # ?
        "1"             # ?
        "2"             # ?
        "1.25 0.65"     # Geometry? (Hardcoded in run_all.sh)
        "6"             # ?
        "0"             # ?
        "0"             # ?
    )
    
    # Create all input files first
    echo "Creating input files..."
    for i in $(seq 1 416); do
        padded_i=$(printf "%03d" $i)
        short_name="s${state_id}t${padded_i}"
        filename="$inputs_dir/input.$i"
        
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
        } > "$filename"
    done
    
    echo "Running 416 KDUQ calculations with $MAX_JOBS parallel jobs..."
    start_time=$(date +%s)
    
    # Run in batches to limit concurrent jobs
    for batch_start in $(seq 1 $MAX_JOBS 416); do
        batch_end=$((batch_start + MAX_JOBS - 1))
        if [ $batch_end -gt 416 ]; then
            batch_end=416
        fi
        
        # Launch batch of jobs
        for i in $(seq $batch_start $batch_end); do
            padded_i=$(printf "%03d" $i)
            short_name="s${state_id}t${padded_i}"
            
            # Run calculation in background
            (
                job_workdir="$workdir/job_$i"
                mkdir -p "$job_workdir"
                cd "$job_workdir"
                
                ../../FRONT_KDUQ < "../../$inputs_dir/input.$i" > /dev/null 2>&1
                echo "tran.$short_name" | ../../TWOFNR > /dev/null 2>&1
                mv *${short_name}* "../../$outputs_dir/" 2>/dev/null
                
                cd ../..
                rm -rf "$job_workdir"
            ) &
        done
        
        # Wait for this batch to complete
        wait
        
        # Progress reporting
        echo "  Progress: $batch_end/416"
    done
    elapsed=$((end_time - start_time))
    
    # Count output files
    output_count=$(ls "$outputs_dir"/21.s${state_id}t* 2>/dev/null | wc -l | xargs)
    
    echo "  ✓ State $state_id complete in ${elapsed}s!"
    echo "  Created $output_count output files in: $outputs_dir"
    echo ""
    
    # Cleanup work directory
    rm -rf "$workdir"
done

echo "=========================================="
echo "All states completed!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "  1. Run: python3 process_multistate.py"
echo "  2. Run: python3 plot_multistate.py"
