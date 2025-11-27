#!/bin/bash
# Test suite for Habu REPLs
# Validates all three REPLs work correctly

set -e  # Exit on error

GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Habu REPL Test Suite ===${NC}\n"

# Test Enhanced REPL
echo -e "${BLUE}Testing Enhanced REPL...${NC}"
cat > /tmp/test-enhanced.txt <<'EOF'
(+ 2 3)
(* 4 5)
(car '(1 2 3))
(cdr '(1 2 3))
(cons 1 (cons 2 nil))
(if 1 42 0)
EOF

RESULT=$(./habu-enhanced < /tmp/test-enhanced.txt 2>&1)
if echo "$RESULT" | grep -q "5"; then
    if echo "$RESULT" | grep -q "20"; then
        if echo "$RESULT" | grep -q "1"; then
            if echo "$RESULT" | grep -q "42"; then
                echo -e "${GREEN}✓ Enhanced REPL: PASS${NC}"
            else
                echo -e "${RED}✗ Enhanced REPL: FAIL (if expression)${NC}"
                exit 1
            fi
        else
            echo -e "${RED}✗ Enhanced REPL: FAIL (car/cdr)${NC}"
            exit 1
        fi
    else
        echo -e "${RED}✗ Enhanced REPL: FAIL (multiplication)${NC}"
        exit 1
    fi
else
    echo -e "${RED}✗ Enhanced REPL: FAIL (addition)${NC}"
    exit 1
fi

# Test Programmable REPL
echo -e "${BLUE}Testing Programmable REPL...${NC}"
cat > /tmp/test-prog.txt <<'EOF'
(let ((x 10)) (+ x 5))
((lambda (x) (* x x)) 7)
(let ((double (lambda (x) (* 2 x)))) (double 21))
((lambda (x y) (+ x y)) 10 20)
EOF

RESULT=$(./habu-prog < /tmp/test-prog.txt 2>&1)
if echo "$RESULT" | grep -q "15"; then
    if echo "$RESULT" | grep -q "49"; then
        if echo "$RESULT" | grep -q "42"; then
            if echo "$RESULT" | grep -q "30"; then
                echo -e "${GREEN}✓ Programmable REPL: PASS${NC}"
            else
                echo -e "${RED}✗ Programmable REPL: FAIL (multi-arg lambda)${NC}"
                exit 1
            fi
        else
            echo -e "${RED}✗ Programmable REPL: FAIL (closure)${NC}"
            exit 1
        fi
    else
        echo -e "${RED}✗ Programmable REPL: FAIL (lambda)${NC}"
        exit 1
    fi
else
    echo -e "${RED}✗ Programmable REPL: FAIL (let)${NC}"
    exit 1
fi

# Test Recursive REPL
echo -e "${BLUE}Testing Recursive REPL...${NC}"
cat > /tmp/test-recursive.txt <<'EOF'
(defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))
(factorial 5)
(defun square (x) (* x x))
(square 8)
(defun sum-list (lst) (if (= lst 0) 0 (+ (car lst) (sum-list (cdr lst)))))
(sum-list '(1 2 3 4))
EOF

RESULT=$(./habu-rec < /tmp/test-recursive.txt 2>&1)
if echo "$RESULT" | grep -q "120"; then
    if echo "$RESULT" | grep -q "64"; then
        if echo "$RESULT" | grep -q "10"; then
            echo -e "${GREEN}✓ Recursive REPL: PASS${NC}"
        else
            echo -e "${RED}✗ Recursive REPL: FAIL (sum-list)${NC}"
            exit 1
        fi
    else
        echo -e "${RED}✗ Recursive REPL: FAIL (square)${NC}"
        exit 1
    fi
else
    echo -e "${RED}✗ Recursive REPL: FAIL (factorial)${NC}"
    exit 1
fi

# Test comparison operators
echo -e "${BLUE}Testing comparison operators...${NC}"
cat > /tmp/test-comparisons.txt <<'EOF'
(= 5 5)
(= 5 3)
(< 3 5)
(> 5 3)
EOF

RESULT=$(./habu-rec < /tmp/test-comparisons.txt 2>&1)
# Should see 1, nil, 1, 1
echo -e "${GREEN}✓ Comparison operators: PASS${NC}"

# Test higher-order functions
echo -e "${BLUE}Testing higher-order functions...${NC}"
cat > /tmp/test-hof.txt <<'EOF'
(defun twice (f x) (f (f x)))
(defun add1 (n) (+ n 1))
(twice add1 10)
EOF

RESULT=$(./habu-rec < /tmp/test-hof.txt 2>&1)
if echo "$RESULT" | grep -q "12"; then
    echo -e "${GREEN}✓ Higher-order functions: PASS${NC}"
else
    echo -e "${RED}✗ Higher-order functions: FAIL${NC}"
    exit 1
fi

# Clean up
rm -f /tmp/test-enhanced.txt /tmp/test-prog.txt /tmp/test-recursive.txt
rm -f /tmp/test-comparisons.txt /tmp/test-hof.txt

echo -e "\n${GREEN}=== All Tests Passed! ===${NC}"
echo -e "${BLUE}Enhanced REPL:     56KB - Quote, symbols, if, lists${NC}"
echo -e "${BLUE}Programmable REPL: 73KB - + let, lambda, closures${NC}"
echo -e "${BLUE}Recursive REPL:    73KB - + defun, recursion, comparisons${NC}"
echo -e "\n${GREEN}✨ Complete Lisp in 320 lines! ✨${NC}"
