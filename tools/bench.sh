#!/bin/sh
# bench.sh — fixed kernels on bin/hb; informs the regalloc tier decisions
# (pool width vs locals cache vs float pool). Prints ns/iter; gates nothing.
# Float kernels join when the d-reg pool work starts (dot caf-1002e9de).
cd "$(dirname "$0")/.."
[ -x bin/hb ] || { echo "no bin/hb — run tools/build.sh"; exit 1; }
python3 - <<'EOF'
import subprocess, time

KERNELS = [
    # name, iters, program  (results must not be constant-foldable)
    ("counter   (reg loop)      ", 100_000_000,
     ": K 0 begin 1 + dup 100000000 = until drop ; K"),
    ("do-loop   (frame stack)   ", 100_000_000,
     ": K 100000000 0 do loop ; K"),
    ("local-ref (ldr per use)   ", 100_000_000,
     ": K {: a :} 0 begin a + dup 100000000 < 0= until drop ; 1 K"),
    ("call      (bl/ret + spill)", 10_000_000,
     ": F 1 + ; : K 0 begin F dup 10000000 = until drop ; K"),
    # float accumulator: d-reg resident across back edges (was 8.55 ns/iter
    # when every f+ round-tripped the memory stack)
    ("f-accum   (d-reg loop)     ", 100_000_000,
     ": K {: n :} 0.0 0 begin 1 + swap 1.5 f+ swap dup n = until drop f0< ; 100000000 K"),
]

for name, iters, prog in KERNELS:
    t0 = time.perf_counter()
    r = subprocess.run(['bin/hb'], input=prog.encode(), capture_output=True)
    dt = time.perf_counter() - t0
    if r.returncode != 0:
        print(f"{name}  FAILED rc={r.returncode}")
        continue
    print(f"{name} {dt:7.3f} s   {dt / iters * 1e9:6.2f} ns/iter")
EOF
