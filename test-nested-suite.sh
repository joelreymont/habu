#!/bin/bash
# Test suite for nested expressions

echo "========================================="
echo "Nested Expression Compilation Test Suite"
echo "========================================="
echo ""

PASS=0
FAIL=0

test_case() {
    local name="$1"
    local ir="$2"
    local expected="$3"

    echo -n "Testing $name... "

    # Generate assembly
    ./ir-to-asm-v2 "$ir" > test-tmp.s 2>&1
    if [ $? -ne 0 ]; then
        echo "FAIL (asm generation)"
        FAIL=$((FAIL + 1))
        return
    fi

    # Assemble
    clang -o test-tmp test-tmp.s 2>&1 > /dev/null
    if [ $? -ne 0 ]; then
        echo "FAIL (assembly)"
        FAIL=$((FAIL + 1))
        return
    fi

    # Run
    ./test-tmp
    result=$?

    if [ $result -eq $expected ]; then
        echo "PASS (got $result)"
        PASS=$((PASS + 1))
    else
        echo "FAIL (expected $expected, got $result)"
        FAIL=$((FAIL + 1))
    fi

    # Cleanup
    rm -f test-tmp test-tmp.s
}

echo "1. Simple Nested Expressions"
echo "-----------------------------"
test_case "1 + (2 + 3)" \
    "(call + (lit 1) (call + (lit 2) (lit 3)))" \
    6

test_case "(1 + 2) + 3" \
    "(call + (call + (lit 1) (lit 2)) (lit 3))" \
    6

test_case "10 - (3 + 2)" \
    "(call - (lit 10) (call + (lit 3) (lit 2)))" \
    5

test_case "(10 - 3) - 2" \
    "(call - (call - (lit 10) (lit 3)) (lit 2))" \
    5

echo ""
echo "2. Nested Multiplication"
echo "------------------------"
test_case "2 * (3 + 4)" \
    "(call * (lit 2) (call + (lit 3) (lit 4)))" \
    14

test_case "(2 + 3) * 4" \
    "(call * (call + (lit 2) (lit 3)) (lit 4))" \
    20

test_case "(2 + 3) * (4 + 3)" \
    "(call * (call + (lit 2) (lit 3)) (call + (lit 4) (lit 3)))" \
    35

echo ""
echo "3. Complex Nested Expressions"
echo "------------------------------"
test_case "2*3 + 4*5" \
    "(call + (call * (lit 2) (lit 3)) (call * (lit 4) (lit 5)))" \
    26

test_case "10 - 2*3" \
    "(call - (lit 10) (call * (lit 2) (lit 3)))" \
    4

test_case "2 * (3 + (4 + 5))" \
    "(call * (lit 2) (call + (lit 3) (call + (lit 4) (lit 5))))" \
    24

echo ""
echo "4. Deep Nesting"
echo "---------------"
test_case "((1+2)+3)+4" \
    "(call + (call + (call + (lit 1) (lit 2)) (lit 3)) (lit 4))" \
    10

test_case "1+(2+(3+4))" \
    "(call + (lit 1) (call + (lit 2) (call + (lit 3) (lit 4))))" \
    10

test_case "(2*3)*(4+1)" \
    "(call * (call * (lit 2) (lit 3)) (call + (lit 4) (lit 1)))" \
    30

echo ""
echo "========================================="
echo "Results: $PASS passed, $FAIL failed"
echo "========================================="

if [ $FAIL -eq 0 ]; then
    echo "✅ ALL NESTED EXPRESSION TESTS PASSED!"
    exit 0
else
    echo "❌ SOME TESTS FAILED"
    exit 1
fi
