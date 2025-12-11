#!/bin/bash

# Test state 1 with first 5 parameter sets to verify it works

echo "=========================================="
echo "Testing State 1 Configuration"
echo "=========================================="
echo "Running first 5 KDUQ parameter sets..."
echo ""

# Clean up any existing test files
rm -rf Outputs_state1_test Inputs_state1_test

# Create test directories
mkdir -p Inputs_state1_test
mkdir -p Outputs_state1_test

# State 1 parameters from config
state_id=1
q_value=5.383
l_transfer=2
j_transfer=2.5
nodes=0
final_spin=0.0
target_mass=19
target_z=8
beam_energy=8.0

# Define input parameters
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
  "$final_spin"
  "1"
  "6"
  "1"
  "1"
  "0"
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

# Test with first 5 parameter sets
for i in 1 2 3 4 5; do
    echo "  Testing parameter set $i..."
    
    filename="Inputs_state1_test/input.$i"
    
    # Create input file
    {
        echo "state1_test$i"
        echo "State 1 test - KDUQ set $i"
        
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
    if [ $? -ne 0 ]; then
        echo "  ERROR: FRONT_KDUQ failed for parameter set $i"
        exit 1
    fi
    
    # Run TWOFNR
    tranfile="tran.state1_test$i"
    if [ ! -f "$tranfile" ]; then
        echo "  ERROR: TWOFNR input file not created: $tranfile"
        exit 1
    fi
    
    echo $tranfile > twofnr.input
    ./TWOFNR < twofnr.input > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "  ERROR: TWOFNR failed for parameter set $i"
        exit 1
    fi
    
    # Check that output files were created
    if [ ! -f "21.state1_test$i" ]; then
        echo "  ERROR: TWOFNR output file not created: 21.state1_test$i"
        exit 1
    fi
    
    # Move outputs
    mv *state1_test$i Outputs_state1_test/ 2>/dev/null
    
    echo "  ✓ Parameter set $i successful"
done

echo ""
echo "=========================================="
echo "✓ State 1 test PASSED!"
echo "=========================================="
echo ""
echo "All 5 test parameter sets completed successfully."
echo "State 1 configuration is working correctly."
echo ""
echo "Ready to run full multi-state pipeline!"
