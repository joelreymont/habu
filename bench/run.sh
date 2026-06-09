#!/bin/sh
# run.sh — Phase 0.3 speed gate. Same LCG inner loop three ways; reports ns/iter.
# Native baseline is HAND-WRITTEN ARM64 (inner-loop.s), not C: clang -O2 measures
# clang's optimizer, not what caf emits. NOTE: the LCG is latency-bound and
# UNDERSTATES native — a dispatch-bound loop is the fair gate (see LESSONS.md).
set -e
cd "$(dirname "$0")"
GF="${GFORTH:-$HOME/.local/bin/gforth}"
GFF="${GFORTH_FAST:-$HOME/.local/bin/gforth-fast}"
ITERS=1000000000

echo "== native (hand-written ARM64 — the real instructions caf will emit) =="
# clang is assembler+linker only here; inner-loop.s is pure ARM64 asm, no C.
clang -arch arm64 -nostartfiles -e _main -o /tmp/caf-il-asm inner-loop.s
/tmp/caf-il-asm && rc=0 || rc=$?    # exit code carries the result's low byte
echo "  correctness: exit=$rc (expect 1 = low byte of result)"
python3 - "$ITERS" <<'PY'
import subprocess, time, sys
it = int(sys.argv[1])
ts = []
for _ in range(6):
    t = time.monotonic(); subprocess.run(['/tmp/caf-il-asm']); ts.append(time.monotonic() - t)
print(f"  ns_per_iter={min(ts)*1e9/it:.3f}  (min of 6, external wall; startup ~ms negligible)")
PY

nsper() { awk -v it="$ITERS" '/us=/{for(i=1;i<=NF;i++){if($i~/^us=/)u=substr($i,4)}; printf "  ns_per_iter=%.3f\n", u*1000.0/it}'; }
echo "== gforth (threaded; = caf-checked today) =="
"$GF"  inner-loop.fs -e "BENCH bye" 2>/dev/null | nsper
echo "== gforth-fast =="
"$GFF" inner-loop.fs -e "BENCH bye" 2>/dev/null | nsper
