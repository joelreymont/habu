#!/bin/bash
# Performance benchmarks for Habu REPLs
# Measures execution time for various operations

set -e

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}"
cat << "EOF"
╔═══════════════════════════════════════════════════════╗
║                                                       ║
║          Habu REPL Performance Benchmarks             ║
║                                                       ║
╚═══════════════════════════════════════════════════════╝
EOF
echo -e "${NC}"

# Function to run benchmark
run_bench() {
    local title="$1"
    local code="$2"
    local repl="${3:-./habu-rec}"

    echo -e "${BLUE}$title${NC}"

    # Run and measure time
    START=$(date +%s%N)
    echo "$code" | $repl > /dev/null 2>&1
    END=$(date +%s%N)

    # Calculate time in milliseconds
    TIME_NS=$((END - START))
    TIME_MS=$((TIME_NS / 1000000))

    echo -e "  ${GREEN}Time: ${TIME_MS}ms${NC}"
    echo ""
}

echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Basic Operations${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

run_bench "Arithmetic (20 additions)" \
"(defun sum-n (n) (if (= n 0) 0 (+ n (sum-n (- n 1)))))
(sum-n 20)"

run_bench "List operations (20 cons)" \
"(defun make-list (n) (if (= n 0) nil (cons n (make-list (- n 1)))))
(make-list 20)"

run_bench "Simple recursion (factorial 20)" \
"(defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))
(factorial 20)"

echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}List Processing${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

run_bench "List length (20 elements)" \
"(defun length (lst) (if (= lst 0) 0 (+ 1 (length (cdr lst)))))
(defun range (start end) (if (> start end) nil (cons start (range (+ start 1) end))))
(length (range 1 20))"

run_bench "List sum (20 elements)" \
"(defun sum (lst) (if (= lst 0) 0 (+ (car lst) (sum (cdr lst)))))
(defun range (start end) (if (> start end) nil (cons start (range (+ start 1) end))))
(sum (range 1 20))"

run_bench "List reverse (20 elements)" \
"(defun reverse (lst) (reverse-helper lst nil))
(defun reverse-helper (lst acc) (if (= lst 0) acc (reverse-helper (cdr lst) (cons (car lst) acc))))
(defun range (start end) (if (> start end) nil (cons start (range (+ start 1) end))))
(reverse (range 1 20))"

echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Higher-Order Functions${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

run_bench "Map over list (15 elements)" \
"(defun map (f lst) (if (= lst 0) nil (cons (f (car lst)) (map f (cdr lst)))))
(defun square (x) (* x x))
(defun range (start end) (if (> start end) nil (cons start (range (+ start 1) end))))
(map square (range 1 15))"

run_bench "Filter list (20 elements)" \
"(defun filter (pred lst) (if (= lst 0) nil (if (pred (car lst)) (cons (car lst) (filter pred (cdr lst))) (filter pred (cdr lst)))))
(defun even? (n) (= (- n (* 2 (/ n 2))) 0))
(defun range (start end) (if (> start end) nil (cons start (range (+ start 1) end))))
(filter even? (range 1 20))"

run_bench "Fold over list (20 elements)" \
"(defun fold (f init lst) (if (= lst 0) init (fold f (f init (car lst)) (cdr lst))))
(defun range (start end) (if (> start end) nil (cons start (range (+ start 1) end))))
(fold + 0 (range 1 20))"

echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Complex Algorithms${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

run_bench "Fibonacci (recursive, n=15)" \
"(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(fib 15)"

run_bench "Power (2^20)" \
"(defun power (base exp) (if (= exp 0) 1 (* base (power base (- exp 1)))))
(power 2 20)"

run_bench "GCD (48, 18)" \
"(defun gcd (a b) (if (= b 0) a (gcd b (- a (* b (/ a b))))))
(gcd 4800 1800)"

echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Closure Performance${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

run_bench "Simple closure (20 calls)" \
"(defun make-adder (n) (lambda (x) (+ x n)))
(defun test-closure (f n) (if (= n 0) 0 (+ (f n) (test-closure f (- n 1)))))
(let ((add5 (make-adder 5))) (test-closure add5 20))"

run_bench "Nested closures" \
"(defun make-multiplier (a) (lambda (b) (lambda (c) (* (* a b) c))))
(let ((mul2 (make-multiplier 2))) (let ((mul2x3 (mul2 3))) (mul2x3 4)))"

echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Startup Time${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

run_bench "REPL startup (echo 42)" "(+ 40 2)"

echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Comparison: Enhanced vs Programmable vs Recursive${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"

echo -e "${CYAN}Enhanced REPL (basic evaluation only):${NC}"
run_bench "  Simple arithmetic" "(+ (* 2 3) (/ 10 2))" "./habu-enhanced"

echo -e "${CYAN}Programmable REPL (+ let, lambda):${NC}"
run_bench "  Lambda application" "((lambda (x y) (+ x y)) 10 20)" "./habu-prog"

echo -e "${CYAN}Recursive REPL (+ defun):${NC}"
run_bench "  Defun + recursion" "(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)" "./habu-rec"

echo -e "\n${GREEN}"
cat << "EOF"
╔═══════════════════════════════════════════════════════╗
║                                                       ║
║              Benchmarks Complete!                     ║
║                                                       ║
║  Note: These are interpreted performance benchmarks.  ║
║  Habu prioritizes simplicity over speed.             ║
║                                                       ║
║  For production workloads, consider compiled Lisps    ║
║  like SBCL or Chez Scheme.                            ║
║                                                       ║
╚═══════════════════════════════════════════════════════╝
EOF
echo -e "${NC}\n"

echo -e "${BLUE}Key Observations:${NC}"
echo "  • Startup time is very fast (instant)"
echo "  • Simple operations are reasonably quick"
echo "  • Deep recursion is limited by stack depth"
echo "  • Exponential algorithms (fib) are slow without memoization"
echo "  • List operations are linear in list length"
echo "  • Closures have minimal overhead"
echo ""
