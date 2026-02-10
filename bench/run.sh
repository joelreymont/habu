#!/bin/bash
# Benchmark runner: Habu hoist JIT vs SBCL
# Usage: bench/run.sh [--save baseline_name] [filter]
#
# Runs each benchmark 3 times, takes the median.

set -euo pipefail
cd "$(dirname "$0")/.."

HABU=./zig-out/bin/habu
ITERS=3
SAVE_NAME=""
FILTER=""

while [[ "${1:-}" == "--save" ]]; do
    SAVE_NAME="${2:-baseline}"
    shift 2
done
FILTER="${1:-}"

mkdir -p bench/results

# Build habu
zig build 2>/dev/null

##############################################################################
# Benchmark definitions
##############################################################################

declare -a NAMES SETUP_H BENCH_H SETUP_S BENCH_S

i=0

# --- JIT-compiled recursive benchmarks ---

NAMES[$i]="fib35"
SETUP_H[$i]='(defun fib (n) (declare (type fixnum n) (optimize (speed 3) (safety 0))) (if (<= n 1) n (the fixnum (+ (fib (the fixnum (- n 1))) (fib (the fixnum (- n 2)))))))'
BENCH_H[$i]='(fib 35)'
SETUP_S[$i]='(defun fib (n) (declare (type fixnum n) (optimize (speed 3) (safety 0))) (if (<= n 1) n (the fixnum (+ (fib (the fixnum (- n 1))) (fib (the fixnum (- n 2)))))))'
BENCH_S[$i]='(fib 35)'
i=$((i+1))

NAMES[$i]="tak_x1000"
SETUP_H[$i]='(defun tak (x y z) (declare (type fixnum x y z) (optimize (speed 3) (safety 0))) (if (<= x y) z (tak (tak (the fixnum (- x 1)) y z) (tak (the fixnum (- y 1)) z x) (tak (the fixnum (- z 1)) x y))))'
BENCH_H[$i]='(let ((i 0)) (while (< i 1000) (tak 18 12 6) (setq i (+ i 1))))'
SETUP_S[$i]='(defun tak (x y z) (declare (type fixnum x y z) (optimize (speed 3) (safety 0))) (if (<= x y) z (tak (tak (the fixnum (- x 1)) y z) (tak (the fixnum (- y 1)) z x) (tak (the fixnum (- z 1)) x y))))'
BENCH_S[$i]='(dotimes (i 1000) (tak 18 12 6))'
i=$((i+1))

NAMES[$i]="ack3_10"
SETUP_H[$i]='(defun ack (m n) (declare (optimize (speed 3) (safety 0))) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (t (ack (- m 1) (ack m (- n 1))))))'
BENCH_H[$i]='(ack 3 10)'
SETUP_S[$i]='(defun ack (m n) (declare (optimize (speed 3) (safety 0))) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (t (ack (- m 1) (ack m (- n 1))))))'
BENCH_S[$i]='(ack 3 10)'
i=$((i+1))

# --- JIT-compiled loop benchmarks ---

NAMES[$i]="fixnum_loop_jit"
SETUP_H[$i]='(defun fixnum-loop () (declare (optimize (speed 3) (safety 0))) (let ((i 0) (acc 0)) (while (< i 1000000) (setq acc (+ acc i)) (setq i (+ i 1))) acc))'
BENCH_H[$i]='(fixnum-loop)'
SETUP_S[$i]='(defun fixnum-loop () (declare (type fixnum) (optimize (speed 3) (safety 0))) (let ((i 0) (acc 0)) (declare (type fixnum i acc)) (loop while (< i 1000000) do (incf acc i) (incf i)) acc))'
BENCH_S[$i]='(fixnum-loop)'
i=$((i+1))

NAMES[$i]="mul_accum_jit"
SETUP_H[$i]='(defun mul-accum-100k () (declare (optimize (speed 3) (safety 0))) (let ((j 0) (result 0)) (while (< j 100000) (let ((acc 1) (i 1)) (while (<= i 20) (setq acc (* acc i)) (setq i (+ i 1))) (setq result acc)) (setq j (+ j 1))) result))'
BENCH_H[$i]='(mul-accum-100k)'
SETUP_S[$i]='(defun mul-accum-100k () (declare (optimize (speed 3) (safety 0))) (let ((j 0) (result 0)) (declare (type fixnum j result)) (loop while (< j 100000) do (let ((acc 1) (i 1)) (declare (type fixnum acc i)) (loop while (<= i 20) do (setf acc (* acc i)) (incf i)) (setf result acc)) (incf j)) result))'
BENCH_S[$i]='(mul-accum-100k)'
i=$((i+1))

NAMES[$i]="nested_loop_jit"
SETUP_H[$i]='(defun nested-loop () (declare (optimize (speed 3) (safety 0))) (let ((sum 0) (i 0)) (while (< i 1000) (let ((j 0)) (while (< j 1000) (setq sum (+ sum (* i j))) (setq j (+ j 1)))) (setq i (+ i 1))) sum))'
BENCH_H[$i]='(nested-loop)'
SETUP_S[$i]='(defun nested-loop () (declare (optimize (speed 3) (safety 0))) (let ((sum 0)) (declare (type fixnum sum)) (dotimes (i 1000) (dotimes (j 1000) (incf sum (* i j)))) sum))'
BENCH_S[$i]='(nested-loop)'
i=$((i+1))

# --- Interpreter-only benchmarks (no JIT) ---

NAMES[$i]="fib30_interp"
SETUP_H[$i]='(defun fib-u (n) (if (<= n 1) n (+ (fib-u (- n 1)) (fib-u (- n 2)))))'
BENCH_H[$i]='(fib-u 30)'
SETUP_S[$i]='(defun fib-u (n) (if (<= n 1) n (+ (fib-u (- n 1)) (fib-u (- n 2)))))'
BENCH_S[$i]='(fib-u 30)'
i=$((i+1))

NAMES[$i]="list_build_100k"
SETUP_H[$i]=''
BENCH_H[$i]='(let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length xs))'
SETUP_S[$i]=''
BENCH_S[$i]='(let ((xs nil)) (dotimes (i 100000) (push i xs)) (length xs))'
i=$((i+1))

NAMES[$i]="hash_insert_20k"
SETUP_H[$i]=''
BENCH_H[$i]='(let ((h (make-hash-table :size 256)) (i 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (hash-table-count h))'
SETUP_S[$i]=''
BENCH_S[$i]='(let ((h (make-hash-table :size 256))) (dotimes (i 20000) (setf (gethash i h) i)) (hash-table-count h))'
i=$((i+1))

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

printf "%-20s %10s %10s %8s\n" "Benchmark" "Habu(us)" "SBCL(us)" "Ratio" >&2
printf "%-20s %10s %10s %8s\n" "---------" "--------" "--------" "-----" >&2

for ((idx=0; idx<NBENCH; idx++)); do
    name="${NAMES[$idx]}"
    
    # Filter
    if [[ -n "$FILTER" && "$name" != *"$FILTER"* ]]; then continue; fi
    
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
    
    # Ratio (>1 means habu faster)
    if [[ "$s_med" -gt 0 && "$h_med" -gt 0 ]]; then
        ratio=$(echo "scale=2; $s_med / $h_med" | bc)
    else
        ratio="n/a"
    fi
    
    printf "%-20s %10s %10s %8s\n" "$name" "${h_med:-0}" "${s_med:-0}" "$ratio" >&2
done

if [[ -n "$SAVE_NAME" ]]; then
    echo "Results saved conceptually as $SAVE_NAME" >&2
fi
