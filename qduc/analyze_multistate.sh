#!/bin/bash

# QDUC Multi-State Complete Workflow
# Processes all states defined in states.config and creates plots

echo "=========================================="
echo "QDUC Multi-State Complete Workflow"
echo "=========================================="
echo ""

# Step 1: Process all states (mean and sigma calculations)
echo "Step 1: Processing all states..."
python3 process_multistate.py
if [ $? -ne 0 ]; then
    echo "ERROR: Processing failed!"
    exit 1
fi
echo ""

# Step 2: Create multi-state comparison plot
echo "Step 2: Creating multi-state comparison plot..."
python3 plot_multistate.py
if [ $? -ne 0 ]; then
    echo "WARNING: Multi-state plotting failed"
fi
echo ""

# Step 3: Fit and plot each individual state (if experimental data exists)
echo "Step 3: Fitting and plotting individual states..."
if [ -f "experimental_data.dat" ]; then
    # Read states.config to get state IDs
    while IFS='|' read -r state_id rest; do
        # Skip comments and empty lines
        [[ "$state_id" =~ ^#.*$ ]] && continue
        [[ -z "$state_id" ]] && continue
        
        # Trim whitespace
        state_id=$(echo "$state_id" | xargs)
        
        echo "  Fitting state $state_id..."
        python3 fit_and_plot_state.py $state_id
        
    done < states.config
else
    echo "  No experimental data found - skipping fits"
fi

echo ""
echo "=========================================="
echo "Workflow Complete!"
echo "=========================================="
echo ""
echo "Results saved to Results/ directory:"
echo "  - Results/multistate_comparison.png (all states)"
echo "  - Results/fit_state*.png (individual fits)"
echo "  - Results/output_state*.dat (processed data)"
echo ""
