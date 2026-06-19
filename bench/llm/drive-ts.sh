#!/bin/sh
# drive-ts.sh - TypeScript arm (array algorithms). f(a) takes number[]; returns
# number (conv as) or a new number[] (conv aa). Repair on runtime/test failures.
# Uses Bun when available and fails closed when no TypeScript runtime exists.
# Usage: drive-ts.sh <id> <name> <sig> <spec> <conv> <vectors> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
ID=$1 NAME=$2 SIG=$3 SPEC=$4 CONV=$5 VEC=$6 MAXR=${7:-5}
model_init
BUN=${BUN:-bun}
calls=$(ts_test "$CONV" "$VEC")
bench=$(ts_bench "$VEC")
T=$(mktemp -d "${TMPDIR:-/tmp}/dts.XXXXXX"); trap 'rm -rf "$T"' EXIT
printf 'prompt unavailable\n' > "$T/prompt.txt"
printf 'response unavailable\n' > "$T/resp.json"
printf '// no candidate extracted\n' > "$T/f.ts"
printf '// no final bundle\n' > "$T/test.ts"
: > "$T/checker-diagnostics.txt"
: > "$T/repair-packet.json"
: > "$T/test-output.txt"

if ! command -v "$BUN" >/dev/null 2>&1; then
  msg="TypeScript runtime unavailable: bun not found; set BUN to a local TypeScript-capable runtime."
  printf '%s\n' "$msg" > "$T/checker-diagnostics.txt"
  printf '%s\n' "$msg" > "$T/test-output.txt"
  msg_q=$(bench_json_quote "$msg")
  printf '{"type":"typescript_runtime_unavailable","message":%s}\n' "$msg_q" > "$T/repair-packet.json"
  BENCH_PROMPT_FILE=$T/prompt.txt
  BENCH_RAW_RESPONSE_FILE=$T/resp.json
  BENCH_CANDIDATE_FILE=$T/f.ts
  BENCH_CHECKER_DIAGNOSTICS_FILE=$T/checker-diagnostics.txt
  BENCH_REPAIR_PACKET_FILE=$T/repair-packet.json
  BENCH_TEST_OUTPUT_FILE=$T/test-output.txt
  BENCH_FINAL_BUNDLE_FILE=$T/test.ts
  BENCH_SOURCE_FILE=$T/f.ts
  emit_row "$ID" "$NAME" "$MODEL" "ts" error 0 0 0 null unavailable
  exit 0
fi

TASK="Write a TypeScript function with this exact signature:
  function f(a: number[]): $(ts_ret_type "$CONV") { ... }
where a is an array of integers. It must return $(ts_ret "$CONV"). Use integer
arithmetic. ${SPEC}
Output ONLY the function definition. No prose, no code fences."
extract() { sed 's/^```.*$//' "$1"; }

round=0; feedback=""; outcome=fail; toks=0; t0=$(now_ms)
while [ "$round" -lt "$MAXR" ]; do
  round=$((round+1))
  prompt="${TASK}${feedback}"
  printf '%s' "$prompt" > "$T/prompt.txt"
  model_run "$prompt" "$T/resp.json" \
    || { printf 'model_run_failed\n' > "$T/resp.json"; outcome=error; break; }
  rt=$(sh bench/llm/parse-resp.sh "$T/resp.json" "$T/text.txt" "$MODEL_PARSER" "$MODEL_TOKEN_FIELDS"); toks=$((toks+rt))
  extract "$T/text.txt" > "$T/f.ts"
  [ -s "$T/f.ts" ] || printf '// no candidate extracted\n' > "$T/f.ts"
  {
    cat "$T/f.ts"; printf '\n'
    printf 'function check(g: unknown, w: unknown, a: string): void {\n'
    printf '  if (JSON.stringify(g) !== JSON.stringify(w)) {\n'
    printf '    console.error("FAIL f(" + a + ") = " + JSON.stringify(g) + " expected " + JSON.stringify(w));\n'
    printf '    process.exit(1);\n'
    printf '  }\n'
    printf '}\n'
    printf '%s\n' "$calls"
    printf 'console.log("ALL-OK");\n'
  } > "$T/test.ts"
  set +e; out=$(timeout 5 "$BUN" "$T/test.ts" 2>&1); rc=$?; set -e
  printf '%s\n' "$out" > "$T/test-output.txt"
  if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -q ALL-OK; then outcome=pass; break
  elif [ "$rc" -eq 124 ]; then outcome=timeout
  else outcome=fail; fi
  feedback="

Your attempt:
$(cat "$T/f.ts")

It FAILED:
$(printf '%s' "$out" | head -8)

Fix it. Output ONLY the corrected function."
done
wall=$(( $(now_ms) - t0 ))
rt_ms=null; rt_status=not_run
if [ "$outcome" = pass ]; then
  {
    cat "$T/f.ts"; printf '\n'
    printf '%s\n' "$bench"
  } > "$T/runtime.ts"
  if rt=$(timeout 5 "$BUN" "$T/runtime.ts" 2>/dev/null | bench_runtime_ms_from_output); then
    rt_ms=$rt; rt_status=ok
  else
    outcome=error; rt_status=error
  fi
fi
BENCH_PROMPT_FILE=$T/prompt.txt
BENCH_RAW_RESPONSE_FILE=$T/resp.json
BENCH_CANDIDATE_FILE=$T/f.ts
BENCH_CHECKER_DIAGNOSTICS_FILE=$T/checker-diagnostics.txt
BENCH_REPAIR_PACKET_FILE=$T/repair-packet.json
BENCH_TEST_OUTPUT_FILE=$T/test-output.txt
BENCH_FINAL_BUNDLE_FILE=$T/test.ts
BENCH_SOURCE_FILE=$T/f.ts
emit_row "$ID" "$NAME" "$MODEL" "ts" "$outcome" "$round" "$toks" "$wall" "$rt_ms" "$rt_status"
