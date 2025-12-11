#!/bin/bash
# Debug script to run one iteration of the parallel logic and capture output

state_id=1
i=1
padded_i="001"
short_name="s1t001"
inputs_dir="Inputs_state1"
outputs_dir="Outputs_state1"
workdir="Work_state1"

mkdir -p "$workdir/job_$i"
cd "$workdir/job_$i"

echo "Running FRONT in $(pwd)..."
../../FRONT_KDUQ < "../../$inputs_dir/input.$i" > front.log 2>&1
cat front.log

echo "Running TWOFNR..."
echo "tran.$short_name" | ../../TWOFNR > twofnr.log 2>&1
cat twofnr.log

cd ../..
