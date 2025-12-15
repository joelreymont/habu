#!/bin/bash
# Regression tests for string operations in habu0
# Run from habu root directory: ./tests/run-string-tests.sh

HABU0=./habu0
PASSED=0
FAILED=0

test_expr() {
    local expr="$1"
    local expected="$2"
    local name="$3"

    echo "$expr" > input.lisp
    $HABU0 2>/dev/null
    local actual=$?

    if [ "$actual" -eq "$expected" ]; then
        echo "✓ $name: $expr -> $actual"
        ((PASSED++))
    else
        echo "✗ $name: $expr -> $actual (expected $expected)"
        ((FAILED++))
    fi
}

echo "========================================"
echo "  Habu0 String Operation Tests"
echo "========================================"
echo ""

# string-ref tests (CRITICAL - known bug with index 1 and 2)
echo "--- string-ref tests: index 0 (usually works) ---"
test_expr '(string-ref "ABC" 0)' 65 "ABC[0] -> A (65)"
test_expr '(string-ref "Hello" 0)' 72 "Hello[0] -> H (72)"
test_expr '(string-ref "01234567" 0)' 48 "01234567[0] -> 0 (48)"
echo ""

echo "--- string-ref tests: index 1 (BUG: returns wrong value) ---"
test_expr '(string-ref "ABC" 1)' 66 "ABC[1] -> B (66)"
test_expr '(string-ref "XY" 1)' 89 "XY[1] -> Y (89)"
test_expr '(string-ref "01234567" 1)' 49 "01234567[1] -> 1 (49)"
echo ""

echo "--- string-ref tests: index 2 (BUG: returns wrong value) ---"
test_expr '(string-ref "ABC" 2)' 67 "ABC[2] -> C (67)"
test_expr '(string-ref "XYZ" 2)' 90 "XYZ[2] -> Z (90)"
test_expr '(string-ref "01234567" 2)' 50 "01234567[2] -> 2 (50)"
echo ""

echo "--- string-ref tests: index 3+ (usually works) ---"
test_expr '(string-ref "XYZW" 3)' 87 "XYZW[3] -> W (87)"
test_expr '(string-ref "Hello" 4)' 111 "Hello[4] -> o (111)"
test_expr '(string-ref "01234567" 3)' 51 "01234567[3] -> 3 (51)"
test_expr '(string-ref "01234567" 7)' 55 "01234567[7] -> 7 (55)"
echo ""

# string-length tests
echo "--- string-length tests ---"
test_expr '(string-length "")' 0 "empty string length"
test_expr '(string-length "A")' 1 "single char length"
test_expr '(string-length "ABC")' 3 "3 char length"
test_expr '(string-length "Hello")' 5 "5 char length"
echo ""

# Symbol equality tests (depends on string-ref working)
echo "--- symbol equality tests ---"
test_expr '(let ((a (quote car)) (b (quote cdr))) (if (eq a b) 1 0))' 0 "car != cdr"
test_expr '(let ((a (quote foo)) (b (quote foo))) (if (eq a b) 1 0))' 1 "foo == foo"
test_expr '(let ((a (quote abc)) (b (quote def))) (if (eq a b) 1 0))' 0 "abc != def"
echo ""

# car/cdr tests (the original bug symptom)
echo "--- car/cdr tests ---"
test_expr '(car (cons 10 20))' 10 "car of cons"
test_expr '(cdr (cons 10 20))' 20 "cdr of cons"
test_expr '(car (cons 3 7))' 3 "car of cons (3,7)"
test_expr '(cdr (cons 3 7))' 7 "cdr of cons (3,7)"
test_expr '(cadr (cons 1 (cons 2 3)))' 2 "cadr"
test_expr '(cddr (cons 1 (cons 2 3)))' 3 "cddr"
echo ""

# Summary
echo "========================================"
echo "  Results: $PASSED passed, $FAILED failed"
echo "========================================"

# Exit with failure count
exit $FAILED
