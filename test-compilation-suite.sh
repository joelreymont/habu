#!/bin/bash
# Comprehensive test suite for Habu compilation pipeline
#
# Tests: Habu IR → ARM64 Assembly → Native Executable

echo "======================================"
echo "Habu Compilation Pipeline Test Suite"
echo "======================================"
echo ""

PASS=0
FAIL=0

test_case() {
    local name="$1"
    local ir="$2"
    local expected="$3"

    echo -n "Testing $name... "

    # Generate assembly
    ./ir-to-asm "$ir" > test-tmp.s 2>&1
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

echo "1. Literal Values"
echo "-----------------"
test_case "return 0"      "(lit 0)"    0
test_case "return 1"      "(lit 1)"    1
test_case "return 42"     "(lit 42)"   42
test_case "return 100"    "(lit 100)"  100
test_case "return 255"    "(lit 255)"  255

echo ""
echo "2. Addition"
echo "-----------"
test_case "3 + 4"         "(call + (lit 3) (lit 4))"      7
test_case "10 + 15"       "(call + (lit 10) (lit 15))"    25
test_case "100 + 23"      "(call + (lit 100) (lit 23))"   123
test_case "0 + 5"         "(call + (lit 0) (lit 5))"      5

echo ""
echo "3. Subtraction"
echo "--------------"
test_case "10 - 3"        "(call - (lit 10) (lit 3))"     7
test_case "100 - 58"      "(call - (lit 100) (lit 58))"   42
test_case "5 - 5"         "(call - (lit 5) (lit 5))"      0

echo ""
echo "4. Multiplication"
echo "-----------------"
test_case "6 * 7"         "(call * (lit 6) (lit 7))"      42
test_case "10 * 10"       "(call * (lit 10) (lit 10))"    100
test_case "3 * 0"         "(call * (lit 3) (lit 0))"      0
test_case "12 * 5"        "(call * (lit 12) (lit 5))"     60

echo ""
echo "======================================"
echo "Results: $PASS passed, $FAIL failed"
echo "======================================"

if [ $FAIL -eq 0 ]; then
    echo "✅ ALL TESTS PASSED!"
    exit 0
else
    echo "❌ SOME TESTS FAILED"
    exit 1
fi
