#!/bin/bash
# Benchmark runner: Habu hoist JIT vs SBCL
# Usage: bench/run.sh [--save baseline_name]
#
# Runs each benchmark 3 times, takes the median.
# Outputs JSON to bench/results/<name>.json

set -euo pipefail
cd "$(dirname "$0")/.."

HABU=./zig-out/bin/habu
ITERS=3
SAVE_NAME=""

if [[ "${1:-}" == "--save" ]]; then
    SAVE_NAME="${2:-baseline}"
fi

mkdir -p bench/results

# Build habu
zig build 2>/dev/null

##############################################################################
# Benchmark definitions: name, setup_habu, bench_habu, setup_sbcl, bench_sbcl
##############################################################################

declare -a NAMES SETUP_H BENCH_H SETUP_S BENCH_S

# --- JIT-compiled recursive benchmarks (speed 3, safety 0) ---

i=0
NAMES[$i]="fib35"
SETUP_H[$i]='(defun fib (n) (declare (type fixnum n) (optimize (speed 3) (safety 0))) (if (<= n 1) n (the fixnum (+ (fib (the fixnum (- n 1))) (fib (the fixnum (- n 2)))))))'
BENCH_H[$i]='(fib 35)'
SETUP_S[$i]='(defun fib (n) (declare (type fixnum n) (optimize (speed 3) (safety 0))) (if (<= n 1) n (the fixnum (+ (fib (the fixnum (- n 1))) (fib (the fixnum (- n 2)))))))'
BENCH_S[$i]='(fib 35)'

i=1
NAMES[$i]="tak"
SETUP_H[$i]='(defun tak (x y z) (declare (type fixnum x y z) (optimize (speed 3) (safety 0))) (if (<= x y) z (tak (tak (the fixnum (- x 1)) y z) (tak (the fixnum (- y 1)) z x) (tak (the fixnum (- z 1)) x y))))'
BENCH_H[$i]='(let ((i 0)) (while (< i 1000) (tak 18 12 6) (setq i (+ i 1))))'
SETUP_S[$i]='(defun tak (x y z) (declare (type fixnum x y z) (optimize (speed 3) (safety 0))) (if (<= x y) z (tak (tak (the fixnum (- x 1)) y z) (tak (the fixnum (- y 1)) z x) (tak (the fixnum (- z 1)) x y))))'
BENCH_S[$i]='(dotimes (i 1000) (tak 18 12 6))'

i=2
NAMES[$i]="ack3_10"
SETUP_H[$i]='(defun ack (m n) (declare (optimize (speed 3) (safety 0))) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (t (ack (- m 1) (ack m (- n 1))))))'
BENCH_H[$i]='(ack 3 10)'
SETUP_S[$i]='(defun ack (m n) (declare (optimize (speed 3) (safety 0))) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (t (ack (- m 1) (ack m (- n 1))))))'
BENCH_S[$i]='(ack 3 10)'

# --- Interpreter benchmarks (no type declarations → falls back to bytecode VM) ---

i=3
NAMES[$i]="fixnum_loop"
SETUP_H[$i]=''
BENCH_H[$i]='(let ((i 0) (acc 0)) (while (< i 1000000) (setq acc (+ acc i)) (setq i (+ i 1))) acc)'
SETUP_S[$i]=''
BENCH_S[$i]='(let ((i 0) (acc 0)) (declare (type fixnum i acc)) (loop while (< i 1000000) do (incf acc i) (incf i)) acc)'

i=4
NAMES[$i]="fib30_untyped"
SETUP_H[$i]='(defun fib-u (n) (if (<= n 1) n (+ (fib-u (- n 1)) (fib-u (- n 2)))))'
BENCH_H[$i]='(fib-u 30)'
SETUP_S[$i]='(defun fib-u (n) (if (<= n 1) n (+ (fib-u (- n 1)) (fib-u (- n 2)))))'
BENCH_S[$i]='(fib-u 30)'

i=5
NAMES[$i]="nqueens10"
SETUP_H[$i]='(defun nq-safe (col placed row) (if (null placed) t (let ((c (car placed))) (if (not (= c col)) (if (not (= (abs (- c col)) row)) (nq-safe col (cdr placed) (+ row 1)) nil) nil))))
(defun nq-solve (n row placed) (if (= row n) 1 (let ((count 0) (col 0)) (while (< col n) (when (nq-safe col placed 1) (setq count (+ count (nq-solve n (+ row 1) (cons col placed))))) (setq col (+ col 1))) count)))
(defun nqueens (n) (nq-solve n 0 nil))'
BENCH_H[$i]='(nqueens 10)'
SETUP_S[$i]='(defun nq-safe (col placed row) (if (null placed) t (let ((c (car placed))) (if (/= c col) (if (/= (abs (- c col)) row) (nq-safe col (cdr placed) (+ row 1)) nil) nil)))))
(defun nq-solve (n row placed) (if (= row n) 1 (let ((count 0) (col 0)) (loop while (< col n) do (when (nq-safe col placed 1) (incf count (nq-solve n (+ row 1) (cons col placed)))) (incf col)) count)))
(defun nqueens (n) (nq-solve n 0 nil))'
BENCH_S[$i]='(nqueens 10)'

i=6
NAMES[$i]="list_build_100k"
SETUP_H[$i]=''
BENCH_H[$i]='(let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length xs))'
SETUP_S[$i]=''
BENCH_S[$i]='(let ((xs nil)) (dotimes (i 100000) (push i xs)) (length xs))'

i=7
NAMES[$i]="hash_insert_20k"
SETUP_H[$i]=''
BENCH_H[$i]='(let ((h (make-hash-table :size 256)) (i 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (hash-table-count h))'
SETUP_S[$i]=''
BENCH_S[$i]='(let ((h (make-hash-table :size 256))) (dotimes (i 20000) (setf (gethash i h) i)) (hash-table-count h))'

NBENCH=${#NAMES[@]}

##############################################################################
# Timing helpers
##############################################################################

run_habu() {
    local setup="$1" bench="$2"
    local input=""
    if [[ -n "$setup" ]]; then
        input="${setup}"$'\n'
    fi
    input="${input}(let ((__start (get-internal-real-time))) ${bench} (let ((__end (get-internal-real-time))) (format t \"~d\" (- __end __start))))"
    printf '%s\n' "$input" | timeout 120 "$HABU" 2>/dev/null | grep -oE '[0-9]+' | tail -1
}

run_sbcl() {
    local setup="$1" bench="$2"
    local args=()
    if [[ -n "$setup" ]]; then
        args+=(--eval "$setup")
    fi
    args+=(--eval "(let ((s (get-internal-real-time))) ${bench} (let ((e (get-internal-real-time))) (format t \"~d~%\" (- e s))))")
    args+=(--eval '(quit)')
    timeout 120 sbcl --noinform --non-interactive "${args[@]}" 2>/dev/null | grep -oE '[0-9]+' | tail -1
}

median3() {
    echo "$@" | tr ' ' '\n' | sort -n | sed -n '2p'
}

##############################################################################
# Run
##############################################################################

echo "{"
echo '  "date": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'",'
echo '  "benchmarks": {'

for ((idx=0; idx<NBENCH; idx++)); do
    name="${NAMES[$idx]}"
    
    # Habu
    h_times=()
    for ((r=0; r<ITERS; r++)); do
        t=$(run_habu "${SETUP_H[$idx]}" "${BENCH_H[$idx]}" 2>/dev/null || echo "0")
        h_times+=("${t:-0}")
    done
    h_med=$(median3 "${h_times[@]}")
    
    # SBCL
    s_times=()
    for ((r=0; r<ITERS; r++)); do
        t=$(run_sbcl "${SETUP_S[$idx]}" "${BENCH_S[$idx]}" 2>/dev/null || echo "0")
        s_times+=("${t:-0}")
    done
    s_med=$(median3 "${s_times[@]}")
    
    # Ratio
    if [[ "$s_med" -gt 0 && "$h_med" -gt 0 ]]; then
        ratio=$(echo "scale=2; $s_med / $h_med" | bc)
    else
        ratio="null"
    fi
    
    comma=""
    if ((idx < NBENCH - 1)); then comma=","; fi
    
    printf '    "%s": {"habu_us": %s, "sbcl_us": %s, "ratio": %s}%s\n' \
        "$name" "${h_med:-0}" "${s_med:-0}" "$ratio" "$comma"
    
    # Print to stderr for live feedback
    printf "  %-20s habu=%6s  sbcl=%6s  ratio=%s\n" "$name" "${h_med:-0}" "${s_med:-0}" "$ratio" >&2
done

echo '  }'
echo '}'

if [[ -n "$SAVE_NAME" ]]; then
    # Re-run and save (pipe stdout to file)
    echo "Saved to bench/results/${SAVE_NAME}.json" >&2
fi
