#!/bin/bash
# Create a local, signed copy of SBCL for JIT compilation

set -e

echo "========================================="
echo "  Creating Local Signed SBCL"
echo "========================================="
echo ""

# Find system SBCL
SYSTEM_SBCL=$(which sbcl)

if [ -z "$SYSTEM_SBCL" ]; then
    echo "Error: SBCL not found in PATH"
    exit 1
fi

echo "Found system SBCL at: $SYSTEM_SBCL"

# Create local bin directory
mkdir -p bin

# Copy SBCL to local directory
echo "Copying SBCL to ./bin/sbcl..."
cp "$SYSTEM_SBCL" bin/sbcl

# Also copy the SBCL core file
CORE_PATH=$(dirname "$SYSTEM_SBCL")/../lib/sbcl/sbcl.core
if [ -f "$CORE_PATH" ]; then
    echo "Copying SBCL core..."
    mkdir -p bin/lib
    cp "$CORE_PATH" bin/lib/sbcl.core
fi

# Sign the local copy (no sudo needed!)
echo ""
echo "Signing local SBCL with JIT entitlements..."
codesign -s - \
    --entitlements jit-entitlements.plist \
    --force \
    bin/sbcl

echo ""
echo "✓ Local SBCL created and signed!"
echo ""

# Verify
echo "Verifying signature..."
codesign -d --entitlements - bin/sbcl 2>&1
echo ""

echo "========================================="
echo "  Done! Now run:"
echo "  ./bin/sbcl --script bootstrap/test-executor.lisp"
echo "========================================="
