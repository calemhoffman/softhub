#!/bin/bash

# Manually reprocess states 1 and 2 that failed TWOFNR

echo "Re-running TWOFNR for states 1 and 2..."

for state_id in 1 2; do
    echo ""
    echo "Processing state $state_id..."
    outputs_dir="Outputs_state${state_id}"
    
    # Find all tran files for this state
    tran_files=$(ls $outputs_dir/tran.state${state_id}_test* 2>/dev/null)
    
    if [ -z "$tran_files" ]; then
        echo "  ERROR: No tran files found in $outputs_dir"
        continue
    fi
    
    count=0
    total=$(ls $outputs_dir/tran.state${state_id}_test* | wc -l)
    
    for tranfile in $outputs_dir/tran.state${state_id}_test*; do
        count=$((count + 1))
        if [ $(($count % 50)) -eq 0 ]; then
            echo "  Progress: $count/$total"
        fi
        
        # Copy tran file to current directory
        basename_tran=$(basename $tranfile)
        cp $tranfile .
        
        # Run TWOFNR
        echo $basename_tran | ./TWOFNR > /dev/null 2>&1
        
        # Move output files back to outputs directory
        mv 20.* $outputs_dir/ 2>/dev/null
        mv 21.* $outputs_dir/ 2>/dev/null
        
        # Clean up temp tran file
        rm -f $basename_tran
    done
    
    echo "  ✓ State $state_id complete! Processed $count files"
    
done

echo ""
echo "Done! Now run:"
echo "  python3 process_multistate.py"
echo "  python3 plot_multistate.py"
