#!/bin/bash

# Reccomen that you run through the FRONT input once by hand, selecting the options 
# you want, then look at in.front file for the values to fill in here
# This one is 19O(d,p) at 8 MeV for d5/2 transfer to g.s. of 20O
# Mod for 132Sn

# First set of "constant" input lines
fixed_lines1=(
  "2"
  "0"
  "0"
  "8.00"
  "36 16"
  "1"
  "1"
  "0 0 0"
  "3 3.5" #l and j
  "0" #nodes
  "2"
  "2.079" #Q value
  "1"
  "0" #132Sn spin
  "1"
  "3"
  "4" #deuteron wf
  "1"
  "3.5" #spin in outgoing
  "1"
  "7" #KDUQ
)

# Second set of "constant" input lines
fixed_lines2=(
  "5" #KDUQ
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

# Loop from 1 to 416
# Example directory path
inputs_dir="Inputs"
outputs_dir="Outputs"
# Create it if it doesn’t exist
mkdir -p "$inputs_dir"
mkdir -p "$outputs_dir"

for i in $(seq 1 416); do

  filename="$inputs_dir/input.$i"

  # Create the file in.front
  rm input.front
  {

    # Info lines (file appendix and description)
    echo "test$i"
    echo "Running with KDUQ parameter set $i"

    # Print first set of fixed lines
    for line in "${fixed_lines1[@]}"; do
      echo "$line"
    done

    # Add the increasing line
    echo "$i"
    echo "5"
    echo "$i"

    # Print second set of fixed lines
    for line in "${fixed_lines2[@]}"; do
      echo "$line"
    done


  } > $filename

  # Run the adapted FRONT program
  ./FRONT_KDUQ < $filename

  ##mv tran* front_outputs/

  tranfile="tran.test$i"
  twofnr_input="twofnr.input"

  # tran file name
  echo $tranfile > $twofnr_input




  ./TWOFNR < $twofnr_input

  mv *.test* $outputs_dir


done


