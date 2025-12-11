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

# 1. Process 416 calculation outputs for all states (Calculate Mean/Sigma)
echo "--------------------------------------------------------"
echo "Step 1: Processing Raw Calculations"
echo "--------------------------------------------------------"
python3 process_multistate.py

# 2. Perform Independent Fits for ALL states
echo "--------------------------------------------------------"
echo "Step 2: Performing Independent Fits (States 1-9)"
echo "--------------------------------------------------------"
for i in {1..9}; do
    echo "  Fitting State $i..."
    python3 fit_and_plot_state.py $i > /dev/null
done

# 3. Aggregate Independent Fit Results
echo "--------------------------------------------------------"
echo "Step 3: Aggregating Fit Statistics"
echo "--------------------------------------------------------"
python3 collect_independent_fits.py

# 4. Generate Independent Summary Plot
echo "--------------------------------------------------------"
echo "Step 4: Generating Independent Summary Visualization"
echo "--------------------------------------------------------"
python3 plot_multistate_independent.py

echo "--------------------------------------------------------"
echo "Analysis Complete! Check Results/ directory."
echo "--------------------------------------------------------"

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
