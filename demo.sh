#!/bin/bash
# Interactive demo of Habu Lisp capabilities

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

clear

echo -e "${CYAN}"
cat << "EOF"
╔═══════════════════════════════════════════════════════╗
║                                                       ║
║          Welcome to Habu Lisp Demo!                   ║
║                                                       ║
║     A Complete Lisp in 73KB / 320 Lines               ║
║                                                       ║
╚═══════════════════════════════════════════════════════╝
EOF
echo -e "${NC}"

sleep 1

# Function to run a demo section
run_demo() {
    local title="$1"
    local code="$2"
    local description="$3"

    echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}$title${NC}"
    if [ -n "$description" ]; then
        echo -e "${NC}$description${NC}"
    fi
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

    echo -e "${GREEN}Input:${NC}"
    echo "$code" | while IFS= read -r line; do
        echo "  $line"
    done
    echo ""

    echo -e "${GREEN}Output:${NC}"
    echo "$code" | ./habu-rec 2>&1 | grep -v "Habu REPL" | grep -v "Features:" | grep -v "Goodbye" | grep -v "^$" | sed 's/habu> /  /' | grep -v "^  $"
    echo ""

    sleep 1
}

# Demo 1: Basic Arithmetic
run_demo "1. Basic Arithmetic" \
"(+ 2 3)
(* 4 5)
(/ 100 4)
(- 50 13)" \
"The foundation of any Lisp - simple arithmetic expressions"

# Demo 2: Lists
run_demo "2. List Operations" \
"'(1 2 3 4 5)
(car '(apple banana cherry))
(cdr '(10 20 30))
(cons 0 '(1 2 3))" \
"Lists are the core data structure in Lisp"

# Demo 3: Conditionals
run_demo "3. Conditional Logic" \
"(if (> 5 3) 'yes 'no)
(if (= 10 10) 42 0)
(if nil 'unreachable 'reached)" \
"Make decisions based on conditions"

# Demo 4: Local Variables
run_demo "4. Local Variables with Let" \
"(let ((x 10) (y 20)) (+ x y))
(let ((radius 5))
  (let ((pi 314))
    (* pi (* radius radius))))" \
"Create local bindings for intermediate values"

# Demo 5: Anonymous Functions
run_demo "5. Anonymous Functions (Lambda)" \
"((lambda (x) (* x x)) 7)
((lambda (x y) (+ (* x x) (* y y))) 3 4)" \
"Functions as first-class values - the heart of functional programming"

# Demo 6: Named Functions
run_demo "6. Named Functions (Defun)" \
"(defun square (x) (* x x))
(square 9)
(defun sum-of-squares (a b) (+ (square a) (square b)))
(sum-of-squares 3 4)" \
"Define reusable functions with names"

# Demo 7: Recursion
run_demo "7. Recursive Functions" \
"(defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))
(factorial 5)
(factorial 10)" \
"Functions can call themselves - enabling powerful algorithms"

# Demo 8: Fibonacci
run_demo "8. Fibonacci Sequence" \
"(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(fib 0)
(fib 5)
(fib 10)" \
"Classic recursive algorithm demonstrating the elegance of Lisp"

# Demo 9: List Processing
run_demo "9. Recursive List Processing" \
"(defun length (lst) (if (= lst 0) 0 (+ 1 (length (cdr lst)))))
(length '(a b c d e))
(defun sum (lst) (if (= lst 0) 0 (+ (car lst) (sum (cdr lst)))))
(sum '(10 20 30 40))" \
"Process lists recursively - a fundamental Lisp pattern"

# Demo 10: Higher-Order Functions
run_demo "10. Higher-Order Functions" \
"(defun twice (f x) (f (f x)))
(defun add1 (n) (+ n 1))
(twice add1 10)
(defun map (f lst) (if (= lst 0) nil (cons (f (car lst)) (map f (cdr lst)))))
(map square '(1 2 3 4 5))" \
"Functions that take or return other functions"

# Demo 11: Closures
run_demo "11. Lexical Closures" \
"(let ((x 10))
  ((lambda (y) (+ x y)) 20))
(let ((multiplier 3))
  (let ((triple (lambda (n) (* multiplier n))))
    (triple 7)))" \
"Functions that capture their surrounding environment"

# Demo 12: Practical Example
run_demo "12. Practical Example: Range and Sum" \
"(defun range (start end) (if (> start end) nil (cons start (range (+ start 1) end))))
(range 1 10)
(sum (range 1 100))" \
"Combining concepts to solve real problems"

# Final message
echo -e "\n${CYAN}"
cat << "EOF"
╔═══════════════════════════════════════════════════════╗
║                                                       ║
║              Demo Complete! ✨                        ║
║                                                       ║
║  You've seen:                                         ║
║    • Basic arithmetic and list operations             ║
║    • Conditionals and local variables                 ║
║    • Anonymous and named functions                    ║
║    • Recursion and list processing                    ║
║    • Higher-order functions and closures              ║
║                                                       ║
║  Try it yourself:                                     ║
║    ./habu-rec                                         ║
║                                                       ║
║  Learn more:                                          ║
║    README_REPL.md - Quick start guide                 ║
║    QUICK_REFERENCE.md - Syntax reference              ║
║    examples.lisp - 100+ example programs              ║
║    stdlib.lisp - Standard library                     ║
║                                                       ║
║  Happy Lisping! 🎉                                    ║
║                                                       ║
╚═══════════════════════════════════════════════════════╝
EOF
echo -e "${NC}\n"
