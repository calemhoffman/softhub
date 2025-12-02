#!/bin/bash
# ANNOTATED VERSION - Shows what to change for ³⁶S(d,p)³⁷S
# This is a reference - modify your actual run_all.sh file

# First set of "constant" input lines
fixed_lines1=(
  "2"
  "0"
  "0"
  
  # ═══════════════════════════════════════════════════════════
  # CHANGE 1: Beam Energy (MeV/nucleon)
  # ═══════════════════════════════════════════════════════════
  "7.65"        # Current: 7.65 MeV for ¹³²Sn
                # Change to: e.g., "10.0" for your ³⁶S experiment
  
  # ═══════════════════════════════════════════════════════════
  # CHANGE 2: Target Nucleus (A Z)
  # ═══════════════════════════════════════════════════════════
  "132 50"      # Current: A=132, Z=50 (Tin)
                # Change to: "36 16" (Sulfur)
  
  "1"
  "1"
  "0 0 0"
  
  # ═══════════════════════════════════════════════════════════
  # CHANGE 3: Transferred Orbital (l j)
  # ═══════════════════════════════════════════════════════════
  "6 6.5"       # Current: l=6, j=6.5 (i13/2 orbital)
                # Change to: e.g., "1 1.5" for d3/2
                #            or    "2 2.5" for d5/2
                #            or    "3 3.5" for f7/2
  
  # ═══════════════════════════════════════════════════════════
  # CHANGE 4: Radial Nodes
  # ═══════════════════════════════════════════════════════════
  "0"           # Current: 0 nodes (1i state)
                # Change to: 0 for 1d, 1 for 2d, etc.
                # Usually keep as "0" for lowest state
  
  "2"
  
  # ═══════════════════════════════════════════════════════════
  # CHANGE 5: Q-value (MeV)
  # ═══════════════════════════════════════════════════════════
  "-2.025"      # Current: Q = -2.025 MeV for ¹³²Sn(d,p)
                # Change to: Calculate from Q = Sn(³⁷S) - 2.224
                # Example: If Sn(³⁷S) = 6.5 MeV, use "4.276"
  
  "1"
  
  # ═══════════════════════════════════════════════════════════
  # CHANGE 6: Target Spin
  # ═══════════════════════════════════════════════════════════
  "0"           # Current: ¹³²Sn is 0⁺
                # Change to: "0" (³⁶S is also 0⁺)
  
  "1"
  "6"
  "4"
  "1"
  
  # ═══════════════════════════════════════════════════════════
  # CHANGE 7: Final State Spin (MUST MATCH CHANGE 3!)
  # ═══════════════════════════════════════════════════════════
  "6.5"         # Current: j = 6.5
                # Change to: SAME as j in CHANGE 3
                # e.g., "1.5" if you used "1 1.5" above
  
  "1"
  "7"
)

# ═══════════════════════════════════════════════════════════════════
# Second set of "constant" input lines - OPTICAL MODEL PARAMETERS
# ═══════════════════════════════════════════════════════════════════
# These control the nuclear optical potential used for distorted waves
# Generally these can stay the same unless you want to use different
# optical model parameters for your specific system
# ═══════════════════════════════════════════════════════════════════

fixed_lines2=(
  # ─────────────────────────────────────────────────────────────────
  # KDUQ Parameter Set Selection (for entrance channel)
  # ─────────────────────────────────────────────────────────────────
  "5"           # KDUQ option for entrance channel
                # Options: 5 = use KDUQ parameter set (recommended)
                #          6 = specify manually
                # This matches line "7" in fixed_lines1
  
  # ─────────────────────────────────────────────────────────────────
  # Entrance Channel Potential Type
  # ─────────────────────────────────────────────────────────────────
  "1"           # Use built-in global optical potential
                # 1 = built-in (recommended)
                # 2 = specify parameters manually
  
  # ─────────────────────────────────────────────────────────────────
  # Exit Channel Potential Selection
  # ─────────────────────────────────────────────────────────────────
  "3"           # Exit channel potential type
                # 1 = built-in global potential
                # 2 = specify manually
                # 3 = use adiabatic deuteron potential (for d,p reactions)
  
  # ─────────────────────────────────────────────────────────────────
  # Adiabatic Model Options (for deuteron channel)
  # ─────────────────────────────────────────────────────────────────
  "1"           # Adiabatic model choice
                # 1 = Johnson-Soper adiabatic (standard)
                # 2 = Watanabe folding model
  
  # ─────────────────────────────────────────────────────────────────
  # Deuteron Wave Function Choice
  # ─────────────────────────────────────────────────────────────────
  "2"           # Deuteron internal wave function
                # 1 = Hulthén wave function
                # 2 = Reid soft-core (recommended)
                # 3 = Paris potential
                # 4 = AV18 potential
  
  # ─────────────────────────────────────────────────────────────────
  # Bound State Potential Parameters
  # ─────────────────────────────────────────────────────────────────
  "1.25 0.65"   # Bound state potential geometry: "r0 a0"
                # r0 = radius parameter (fm)
                #      Radius R = r0 × A^(1/3)
                # a0 = diffuseness parameter (fm)
                # Standard values: r0 ≈ 1.25 fm, a0 ≈ 0.65 fm
                # These define the Woods-Saxon potential well
  
  # ─────────────────────────────────────────────────────────────────
  # Bound State Potential Type
  # ─────────────────────────────────────────────────────────────────
  "6"           # Bound state potential choice
                # 1 = specify depth manually
                # 6 = adjust depth to reproduce separation energy
                #     (recommended - automatically fits to Sn)
  
  # ─────────────────────────────────────────────────────────────────
  # Spin-Orbit Potential
  # ─────────────────────────────────────────────────────────────────
  "0"           # Spin-orbit option
                # 0 = use same geometry as central potential
                # 1 = specify different spin-orbit geometry
                # For most cases, 0 is fine
  
  # ─────────────────────────────────────────────────────────────────
  # Finite Range Parameters (if needed)
  # ─────────────────────────────────────────────────────────────────
  "1.1"         # Finite range parameter D0 (fm)
                # Controls finite-range correction
                # Typical value: 1.1 fm
                # Set to 0.0 to use zero-range approximation
  
  "0.65"        # Finite range parameter β (fm)
                # Related to non-locality range
                # Typical value: 0.65 fm
                # Used in LEA (Local Energy Approximation)
)

# ═══════════════════════════════════════════════════════════════════
# NOTES ON OPTICAL MODEL PARAMETERS
# ═══════════════════════════════════════════════════════════════════
#
# For most (d,p) reactions, the default values above work well:
# - KDUQ provides systematic uncertainties via 416 parameter sets
# - Adiabatic model is standard for deuteron breakup
# - Reid soft-core is a well-tested deuteron wave function
# - Bound state auto-fit (option 6) ensures correct binding
#
# You typically DON'T need to change these unless:
# 1. You want to test different deuteron wave functions
# 2. You have specific optical model parameters from fits
# 3. You're doing sensitivity studies on the potential
#
# The KDUQ uncertainty quantification (416 sets) already accounts
# for optical model uncertainties in the entrance channel.
# ═══════════════════════════════════════════════════════════════════

# ═══════════════════════════════════════════════════════════
# EXAMPLE: Complete modification for ³⁶S(d,p)³⁷S to d3/2
# ═══════════════════════════════════════════════════════════
# Assumptions:
#   - Beam: 10 MeV deuterons
#   - State: 1d3/2 (l=1, j=1.5)
#   - Sn(³⁷S) ≈ 6.5 MeV → Q ≈ 4.28 MeV
#
# Modified fixed_lines1 would be:
# fixed_lines1=(
#   "2"
#   "0"
#   "0"
#   "10.0"        # ← Beam energy
#   "36 16"       # ← Target A, Z
#   "1"
#   "1"
#   "0 0 0"
#   "1 1.5"       # ← l, j for d3/2
#   "0"           # ← Nodes
#   "2"
#   "4.28"        # ← Q-value
#   "1"
#   "0"           # ← Target spin
#   "1"
#   "6"
#   "4"
#   "1"
#   "1.5"         # ← Final spin (= j)
#   "1"
#   "7"
# )
# ═══════════════════════════════════════════════════════════
