#!/bin/sh
# drive-js.sh — JavaScript arm (array algorithms). f(a) takes an array; returns a
# number (conv as) or a new array (conv aa). Repair on test failures (node).
# Usage: drive-js.sh <id> <name> <sig> <spec> <conv> <vectors> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
ID=$1 NAME=$2 SIG=$3 SPEC=$4 CONV=$5 VEC=$6 MAXR=${7:-5}
model_init
calls=$(js_test "$CONV" "$VEC")
bench=$(js_bench "$VEC")
T=$(mktemp -d "${TMPDIR:-/tmp}/djs.XXXXXX"); trap 'rm -rf "$T"' EXIT
printf 'prompt unavailable\n' > "$T/prompt.txt"
printf 'response unavailable\n' > "$T/resp.json"
printf '// no candidate extracted\n' > "$T/f.js"
printf '// no final bundle\n' > "$T/test.js"
: > "$T/checker-diagnostics.txt"
: > "$T/repair-packet.json"
: > "$T/test-output.txt"
TASK="Write a JavaScript function with this exact signature:
  function f(a) { ... }
where a is an array of integers. It must return $(js_ret "$CONV"). Use integer
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
  rt=$(bin/hb --load lib/errors.f lib/string.f lib/fs.f tools/json.f tools/argv.f bench/llm/parse-resp-lib.f bench/llm/parse-resp.f -- "$T/resp.json" "$T/text.txt" "$MODEL_PARSER" "$MODEL_TOKEN_FIELDS"); toks=$((toks+rt))
  extract "$T/text.txt" > "$T/f.js"
  [ -s "$T/f.js" ] || printf '// no candidate extracted\n' > "$T/f.js"
  {
    cat "$T/f.js"; printf '\n'
    printf 'function check(g,w,a){ if(JSON.stringify(g)!==JSON.stringify(w)){ console.error("FAIL f("+a+") = "+JSON.stringify(g)+" expected "+JSON.stringify(w)); process.exit(1);} }\n'
    printf '%s\n' "$calls"
    printf 'console.log("ALL-OK");\n'
  } > "$T/test.js"
  set +e; out=$(timeout 5 node "$T/test.js" 2>&1); rc=$?; set -e
  printf '%s\n' "$out" > "$T/test-output.txt"
  if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -q ALL-OK; then outcome=pass; break
  elif [ "$rc" -eq 124 ]; then outcome=timeout
  else outcome=fail; fi
  feedback="

Your attempt:
$(cat "$T/f.js")

It FAILED:
$(printf '%s' "$out" | head -4)

Fix it. Output ONLY the corrected function."
done
wall=$(( $(now_ms) - t0 ))
rt_ms=null; rt_status=not_run
if [ "$outcome" = pass ]; then
  {
    cat "$T/f.js"; printf '\n'
    printf '%s\n' "$bench"
  } > "$T/runtime.js"
  if rt=$(timeout 5 node "$T/runtime.js" 2>/dev/null | bench_runtime_ms_from_output); then
    rt_ms=$rt; rt_status=ok
  else
    outcome=error; rt_status=error
  fi
fi
BENCH_PROMPT_FILE=$T/prompt.txt
BENCH_RAW_RESPONSE_FILE=$T/resp.json
BENCH_CANDIDATE_FILE=$T/f.js
BENCH_CHECKER_DIAGNOSTICS_FILE=$T/checker-diagnostics.txt
BENCH_REPAIR_PACKET_FILE=$T/repair-packet.json
BENCH_TEST_OUTPUT_FILE=$T/test-output.txt
BENCH_FINAL_BUNDLE_FILE=$T/test.js
BENCH_SOURCE_FILE=$T/f.js
emit_row "$ID" "$NAME" "$MODEL" "js" "$outcome" "$round" "$toks" "$wall" "$rt_ms" "$rt_status"
