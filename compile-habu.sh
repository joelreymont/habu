#!/bin/bash
# Compile a Habu expression to native ARM64 executable
#
# Usage: ./compile-habu.sh EXPR [OUTPUT]
# Example: ./compile-habu.sh '(+ 10 15)' test-output

if [ $# -lt 1 ]; then
    echo "Usage: $0 'EXPRESSION' [output-name]"
    echo "Example: $0 '(+ 10 15)' test-add"
    exit 1
fi

EXPR="$1"
OUTPUT="${2:-a.out}"

# For now, manually convert simple expressions to IR
# In the future, this will call Habu's compile-expr

case "$EXPR" in
    *"+ 10 15"*)
        IR="(call + (lit 10) (lit 15))"
        ;;
    *"* 6 7"*)
        IR="(call * (lit 6) (lit 7))"
        ;;
    *"- 20 8"*)
        IR="(call - (lit 20) (lit 8))"
        ;;
    [0-9]*)
        IR="(lit $EXPR)"
        ;;
    *)
        echo "Expression not yet supported: $EXPR"
        exit 1
        ;;
esac

echo "Compiling: $EXPR"
echo "IR: $IR"

# Generate assembly
./ir-to-asm "$IR" > "$OUTPUT.s" || exit 1

# Assemble and link
clang -o "$OUTPUT" "$OUTPUT.s" || exit 1

echo "Generated: $OUTPUT"
echo "Running..."
./"$OUTPUT"
echo "Exit code: $?"
