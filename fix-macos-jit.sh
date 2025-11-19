#!/bin/bash
# Fix macOS JIT restrictions for SBCL

set -e

echo "========================================="
echo "  Fixing macOS JIT Restrictions"
echo "========================================="
echo ""

# Find SBCL executable
SBCL_PATH=$(which sbcl)

if [ -z "$SBCL_PATH" ]; then
    echo "Error: SBCL not found in PATH"
    exit 1
fi

echo "Found SBCL at: $SBCL_PATH"
echo ""

# Check if already signed
echo "Checking current code signature..."
codesign -d -vv "$SBCL_PATH" 2>&1 || true
echo ""

# Sign with entitlements
echo "Signing SBCL with JIT entitlements..."
codesign -s - \
    --entitlements jit-entitlements.plist \
    --force \
    --deep \
    "$SBCL_PATH"

echo ""
echo "✓ SBCL signed successfully!"
echo ""

# Verify signature
echo "Verifying signature..."
codesign -d --entitlements - "$SBCL_PATH" 2>&1
echo ""

echo "========================================="
echo "  Done! You can now run:"
echo "  sbcl --script bootstrap/test-executor.lisp"
echo "========================================="
