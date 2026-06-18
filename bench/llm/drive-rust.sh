#!/bin/sh
# drive-rust.sh — Rust arm (array algorithms). f(a: &[i64]) returns i64 (conv as)
# or Vec<i64> (conv aa). Repair on rustc errors AND test failures.
# Usage: drive-rust.sh <id> <name> <sig> <spec> <conv> <vectors> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
ID=$1 NAME=$2 SIG=$3 SPEC=$4 CONV=$5 VEC=$6 MAXR=${7:-5}
model_init
asserts=$(rust_test "$CONV" "$VEC")
bench=$(rust_bench "$VEC")
T=$(mktemp -d "${TMPDIR:-/tmp}/drs.XXXXXX"); trap 'rm -rf "$T"' EXIT
printf 'prompt unavailable\n' > "$T/prompt.txt"
printf 'response unavailable\n' > "$T/resp.json"
printf '// no candidate extracted\n' > "$T/f.rs"
printf '// no final bundle\n' > "$T/test.rs"
: > "$T/checker-diagnostics.txt"
: > "$T/repair-packet.json"
: > "$T/test-output.txt"
TASK="Write a Rust function with this exact signature:
  fn f(a: &[i64]) -> $(rust_ret "$CONV") { ... }
where a is a slice of integers. ${SPEC}
Output ONLY the function definition. No prose, no code fences."
extract() { sed 's/^```.*$//' "$1"; }

round=0; feedback=""; outcome=reject; toks=0; t0=$(now_ms)
while [ "$round" -lt "$MAXR" ]; do
  round=$((round+1))
  prompt="${TASK}${feedback}"
  printf '%s' "$prompt" > "$T/prompt.txt"
  model_run "$prompt" "$T/resp.json" \
    || { printf 'model_run_failed\n' > "$T/resp.json"; outcome=error; break; }
  rt=$(node bench/llm/parse-resp.js "$T/resp.json" "$T/text.txt" "$MODEL_PARSER" "$MODEL_TOKEN_FIELDS"); toks=$((toks+rt))
  extract "$T/text.txt" > "$T/f.rs"
  [ -s "$T/f.rs" ] || printf '// no candidate extracted\n' > "$T/f.rs"
  {
    cat "$T/f.rs"; printf '\n'
    printf 'fn main() {\n%s\n    println!("ALL-OK");\n}\n' "$asserts"
  } > "$T/test.rs"
  if ! rustc "$T/test.rs" -o "$T/bin" 2>"$T/comp.err"; then
    outcome=reject
    cp "$T/comp.err" "$T/checker-diagnostics.txt"
    cp "$T/comp.err" "$T/test-output.txt"
    feedback="

Your attempt:
$(cat "$T/f.rs")

rustc REJECTED it:
$(head -8 "$T/comp.err")

Fix it so it compiles. Output ONLY the corrected function."
    continue
  fi
  set +e; out=$(timeout 5 "$T/bin" 2>&1); rc=$?; set -e
  printf '%s\n' "$out" > "$T/test-output.txt"
  if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -q ALL-OK; then outcome=pass; break
  elif [ "$rc" -eq 124 ]; then outcome=timeout
  else outcome=fail; fi
  feedback="

Your attempt:
$(cat "$T/f.rs")

It compiled but FAILED a test:
$(printf '%s' "$out" | grep -i 'assert\|panic\|left\|right' | head -4)

Fix the logic. Output ONLY the corrected function."
done
wall=$(( $(now_ms) - t0 ))
rt_ms=null; rt_status=not_run
if [ "$outcome" = pass ]; then
  {
    cat "$T/f.rs"; printf '\n'
    printf 'fn main() {\n%s\n}\n' "$bench"
  } > "$T/runtime.rs"
  if rustc "$T/runtime.rs" -o "$T/runtime-bin" 2>"$T/runtime-comp.err"; then
    if rt=$(timeout 5 "$T/runtime-bin" 2>/dev/null | bench_runtime_ms_from_output); then
      rt_ms=$rt; rt_status=ok
    else
      outcome=error; rt_status=error
    fi
  else
    outcome=error; rt_status=error
  fi
fi
BENCH_PROMPT_FILE=$T/prompt.txt
BENCH_RAW_RESPONSE_FILE=$T/resp.json
BENCH_CANDIDATE_FILE=$T/f.rs
BENCH_CHECKER_DIAGNOSTICS_FILE=$T/checker-diagnostics.txt
BENCH_REPAIR_PACKET_FILE=$T/repair-packet.json
BENCH_TEST_OUTPUT_FILE=$T/test-output.txt
BENCH_FINAL_BUNDLE_FILE=$T/test.rs
BENCH_SOURCE_FILE=$T/f.rs
emit_row "$ID" "$NAME" "$MODEL" "rust" "$outcome" "$round" "$toks" "$wall" "$rt_ms" "$rt_status"
