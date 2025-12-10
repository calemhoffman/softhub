# Multi-State QDUC Calculation System

## Overview
This system allows you to run QDUC calculations for multiple nuclear states in a single automated workflow.

## Files Created
- **`states.config`** - Configuration file defining all states to calculate
- **`run_all_multistate.sh`** - Master script that runs all calculations
- **`process_multistate.py`** - Post-processing script to analyze results
- **`plot_multistate.py`** - Visualization script to plot all states

## Quick Start

### 1. Configure Your States
Edit `states.config` to define the states you want to calculate:

```bash
# Uncomment and fill in the template lines for states 2 and 3
# Format: state_id | Q_value | l | j | nodes | final_spin | mass | Z | E_beam | description
```

**State 1** is already configured for the O19 ground state.
**States 2 and 3** are provided as templates - uncomment and fill in your values.

### 2. Run Calculations
```bash
./run_all_multistate.sh
```

This will:
- Read `states.config`
- Create separate directories for each state
- Run 416 KDUQ parameter sets per state
- Store outputs in `Outputs_state1/`, `Outputs_state2/`, etc.

**Note:** This can take a long time (45+ minutes per state)

### 3. Process Results
```bash
python3 process_multistate.py
```

This will:
- Analyze outputs for each state
- Calculate mean and standard deviations
- Save results to `Results/output_state1.dat`, `Results/output_state2.dat`, etc.

### 4. Visualize Results
```bash
python3 plot_multistate.py
```

This creates `Results/multistate_comparison.png` showing all states on one plot.

## Directory Structure

```
qduc/
├── states.config              # State configuration file
├── run_all_multistate.sh      # Master execution script
├── process_multistate.py      # Analysis script
├── plot_multistate.py         # Visualization script
│
├── Inputs_state1/             # Input files for state 1
├── Inputs_state2/             # Input files for state 2
│
├── Outputs_state1/            # Raw outputs for state 1
├── Outputs_state2/            # Raw outputs for state 2
│
└── Results/                   # Processed results
    ├── output_state1.dat
    ├── output_state2.dat
    └── multistate_comparison.png
```

## Example: O19(d,p)O20 States

Here are some realistic state parameters for O19:

| State | E_x (MeV) | Q (MeV) | l | j | J^π | Description |
|-------|-----------|---------|---|-----|-----|-------------|
| 1 | 0.000 | 5.383 | 2 | 2.5 | 3/2+ | Ground state |
| 2 | 1.674 | 3.709 | 0 | 0.5 | 1/2+ | 1st excited |
| 3 | 3.570 | 1.813 | 2 | 2.5 | 5/2+ | 2nd excited |

## Adding More States

To add additional states:
1. Copy the template format from `states.config`
2. Add new lines with unique state_id values
3. Fill in all parameters
4. The scripts will automatically process all active states

## Troubleshooting

**No outputs found:**
- Check that FRONT_KDUQ and TWOFNR executables are present
- Verify states.config has uncommented state lines
- Check for error messages in terminal output

**Processing fails:**
- Ensure calculations completed successfully first
- Check that `Outputs_stateX/` directories contain files
- Verify file naming matches pattern `21.stateX_test*`

**Plot is empty:**
- Run `process_multistate.py` before plotting
- Check that `Results/output_stateX.dat` files exist
- Verify at least one state is uncommented in config

## Notes

- Each state runs 416 KDUQ parameter sets independently
- States are calculated sequentially (not in parallel)
- You can interrupt and restart - script will overwrite existing outputs
- Results are saved in separate directories to avoid conflicts
