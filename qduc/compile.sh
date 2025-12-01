#!/bin/bash
# Compilation script for QDUC nuclear physics codes
# Handles macOS Homebrew gfortran linker issues

echo "Compiling QDUC Fortran programs..."
echo ""

# Set library path for macOS
export LIBRARY_PATH=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib

# Compile FRONT_KDUQ
echo "Compiling FRONT_KDUQ from front21_KDUQ.f..."
gfortran -o FRONT_KDUQ front21_KDUQ.f -std=legacy -w
if [ $? -eq 0 ]; then
    echo "✓ FRONT_KDUQ compiled successfully"
else
    echo "✗ FRONT_KDUQ compilation failed"
    exit 1
fi

# Compile TWOFNR
echo "Compiling TWOFNR from twofnr20.f..."
gfortran -o TWOFNR twofnr20.f -std=legacy -w
if [ $? -eq 0 ]; then
    echo "✓ TWOFNR compiled successfully"
else
    echo "✗ TWOFNR compilation failed"
    exit 1
fi

echo ""
echo "Compilation complete!"
echo ""
ls -lh FRONT_KDUQ TWOFNR
