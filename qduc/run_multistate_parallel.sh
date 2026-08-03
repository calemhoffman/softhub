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
    
    # MODIFIED: Always clean and re-run active states to reflect states.config changes
    outputs_dir="sim_data/Outputs_state${state_id}"

    # Create directories and clean old outputs to prevent false positives
    inputs_dir="sim_data/Inputs_state${state_id}"
    workdir="sim_data/Work_state${state_id}"
    rm -rf "$inputs_dir" "$outputs_dir" "$workdir"
    mkdir -p "$inputs_dir" "$outputs_dir" "$workdir"
    
    # Fixed parameters for 36S(d,t)35S. Entrance-channel deuteron potential
    # uses Watanabe folding so the 416 KDUQ nucleon-potential ensemble drives
    # the entrance channel (the triton exit channel has no KDUQ ensemble;
    # it uses the fixed D.Y. Pang GDP08 global potential instead).
    # Sequence verified by probing FRONT_KDUQ interactively for ireac=5.
    fixed_lines1=(
        "5"                         # Reaction type (d,t)
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
        "1"                         # Non-locality, incident (deuteron) channel: no
        "$initial_spin"             # Initial spin
        "1"                         # Incident channel potential: from built-in
        "4"                         # Deuteron potential: [4] Watanabe folding
        "1"                         # Deuteron wave function: [1] Reid soft-core
        "1"                         # Non-locality, outgoing (triton) channel: no
        "$final_spin"               # Target spin outgoing
        "1"                         # Outgoing (triton) channel potential: from built-in
        "3"                         # Triton potential: [3] D.Y. Pang et al. GDP08
        "5"                         # Nucleon potential for folding: [5] KDUQ Democratic global
    )

    fixed_lines2=(
        "1.25 0.65"     # Neutron binding potential: radius, diffuseness (fm)
        "6"             # Spin-orbit: strength of l.sigma (~6.0 MeV)
        "0"             # Bound state non-locality (0 usually)
        "0"             # Bound state spin-orbit radius (0 = use central geometry)
        "0"             # Bound state spin-orbit diffuseness
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

            echo "$i"    # KDUQ sample number (1-416), entrance-channel deuteron potential
            echo "1"     # Use default <d|t> vertex constant D0
            echo "2"     # Finite-range treatment: local-energy (default)

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
                
                ../../../FRONT_KDUQ < "../../../$inputs_dir/input.$i" > /dev/null 2>&1
                echo "tran.$short_name" | ../../../TWOFNR > /dev/null 2>&1
                mv *${short_name}* "../../../$outputs_dir/" 2>/dev/null
                
                cd ../../..
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
