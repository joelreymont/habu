#!/bin/bash
# Benchmark runner - compares habu native vs SBCL compiled performance
# Usage: ./run_benchmarks.sh

set -e
cd "$(dirname "$0")/.."

BENCHMARKS="fib tak list arith"
ITERATIONS=5
RESULTS_FILE="benchmarks/results.txt"

echo "Habu vs SBCL Benchmark Suite"
echo "============================"
echo ""
echo "Date: $(date)"
echo "Platform: $(uname -m)"
echo ""

# Clean previous results
rm -f "$RESULTS_FILE"
mkdir -p /tmp/habu_bench

# Function to time a command (returns milliseconds)
time_cmd() {
    local start end
    start=$(python3 -c "import time; print(int(time.time() * 1000))")
    "$@" >/dev/null 2>&1
    end=$(python3 -c "import time; print(int(time.time() * 1000))")
    echo $((end - start))
}

# Compile and benchmark each test
for bench in $BENCHMARKS; do
    echo "Benchmark: $bench"
    echo "-------------------"

    BENCH_FILE="benchmarks/bench_${bench}.lisp"
    HABU_BIN="/tmp/habu_bench/${bench}_habu"

    if [ ! -f "$BENCH_FILE" ]; then
        echo "  [SKIP] $BENCH_FILE not found"
        continue
    fi

    # Compile with habu
    echo "  Compiling with habu..."
    ./bin/habu <<EOF >/dev/null 2>&1
:compile $BENCH_FILE $HABU_BIN
:quit
EOF

    if [ ! -f "$HABU_BIN" ]; then
        echo "  [ERROR] Failed to compile with habu"
        continue
    fi

    HABU_SIZE=$(stat -f%z "$HABU_BIN" 2>/dev/null || stat -c%s "$HABU_BIN")
    echo "  Habu binary size: $HABU_SIZE bytes"

    # Run habu binary multiple times
    echo "  Running habu binary ($ITERATIONS iterations)..."
    HABU_TOTAL=0
    for i in $(seq 1 $ITERATIONS); do
        ms=$(time_cmd "$HABU_BIN")
        HABU_TOTAL=$((HABU_TOTAL + ms))
    done
    HABU_AVG=$((HABU_TOTAL / ITERATIONS))

    # Run SBCL equivalent
    echo "  Running SBCL ($ITERATIONS iterations)..."
    SBCL_TOTAL=0
    for i in $(seq 1 $ITERATIONS); do
        ms=$(time_cmd sbcl --noinform --non-interactive --load "$BENCH_FILE")
        SBCL_TOTAL=$((SBCL_TOTAL + ms))
    done
    SBCL_AVG=$((SBCL_TOTAL / ITERATIONS))

    # Calculate ratio
    if [ $SBCL_AVG -gt 0 ]; then
        RATIO=$(python3 -c "print(f'{$HABU_AVG / $SBCL_AVG:.2f}')")
    else
        RATIO="N/A"
    fi

    echo "  Results:"
    echo "    Habu native: ${HABU_AVG}ms average"
    echo "    SBCL:        ${SBCL_AVG}ms average"
    echo "    Ratio:       ${RATIO}x (lower is better for habu)"
    echo ""

    # Save to results file
    echo "$bench,$HABU_AVG,$SBCL_AVG,$RATIO,$HABU_SIZE" >> "$RESULTS_FILE"
done

echo "============================"
echo "Summary"
echo "============================"
echo ""
printf "%-10s %10s %10s %10s %12s\n" "Benchmark" "Habu(ms)" "SBCL(ms)" "Ratio" "BinarySize"
echo "---------------------------------------------------------------"
while IFS=, read -r name habu sbcl ratio size; do
    printf "%-10s %10s %10s %10s %12s\n" "$name" "$habu" "$sbcl" "$ratio" "$size"
done < "$RESULTS_FILE"
echo ""
echo "Results saved to $RESULTS_FILE"
