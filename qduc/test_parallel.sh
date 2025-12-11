#!/bin/bash

# Quick test of parallel processing - runs only 16 iterations for state 1
# This tests the parallel infrastructure before full run

CONFIG_FILE="states.config"
NUM_CORES=4  # Use 4 cores for testing
TEST_ITERATIONS=16  # Only run 16 instead of 416

echo "=========================================="
echo "PARALLEL PROCESSING TEST"
echo "=========================================="
echo "Running $TEST_ITERATIONS iterations for state 1 using $NUM_CORES cores"
echo ""

# Read first state only
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
    
    break  # Only first state
done < "$CONFIG_FILE"

echo "Testing STATE $state_id: $description"
echo ""

# Create directories
inputs_dir="Test_Inputs"
outputs_dir="Test_Outputs"
rm -rf "$inputs_dir" "$outputs_dir"
mkdir -p "$inputs_dir" "$outputs_dir"

# Function to run single iteration
run_test_iteration() {
    local i=$1
    local padded_i=$(printf "%03d" $i)
    local short_name="test${padded_i}"
    
    # Simple input file
    cat > "$inputs_dir/input.$i" <<EOF
$short_name
Test iteration $i
2
0
0
$beam_energy
2
1
1
1007.825
1875.613
0
939.5654
0
$target_mass
$target_Z
0
$initial_spin
0
0
1
0
3
$final_spin
0
$l_transfer
$j_transfer
$nodes
$Q_value
$i
5
$i
181
0.
180.
1
0.0001
25.
0.05
EOF
    
    # Run FRONT and TWOFNR
    ./FRONT_KDUQ < "$inputs_dir/input.$i" > /dev/null 2>&1
    echo "tran.$short_name" | ./TWOFNR > /dev/null 2>&1
    mv *${short_name}* "$outputs_dir/" 2>/dev/null
}

export -f run_test_iteration
export inputs_dir outputs_dir state_id beam_energy initial_spin final_spin
export target_mass target_Z l_transfer j_transfer nodes Q_value

echo "Starting parallel test..."
start_time=$(date +%s)

seq 1 $TEST_ITERATIONS | xargs -P $NUM_CORES -I {} bash -c 'run_test_iteration {}'

end_time=$(date +%s)
elapsed=$((end_time - start_time))

# Count results
output_count=$(ls "$outputs_dir"/21.test* 2>/dev/null | wc -l | xargs)

echo ""
echo "=========================================="
echo "Test Results:"
echo "=========================================="
echo "  Time elapsed: ${elapsed}s"
echo "  Expected files: $TEST_ITERATIONS"
echo "  Output files created: $output_count"
echo "  Avg time per iteration: $((elapsed * 1000 / TEST_ITERATIONS))ms"
echo ""

if [ "$output_count" -eq "$TEST_ITERATIONS" ]; then
    echo "✓ SUCCESS! Parallel processing working correctly"
    echo ""
    echo "Estimated time for full run (416 iterations × 9 states):"
    est_time=$((elapsed * 416 / TEST_ITERATIONS * 9 / NUM_CORES * 8 / 4))  # Scale to 8 cores
    est_min=$((est_time / 60))
    echo "  ~${est_min} minutes with 8 cores"
else
    echo "✗ FAILED! Expected $TEST_ITERATIONS files but got $output_count"
fi

echo ""
echo "To run full parallel calculation: ./run_multistate_parallel.sh"
