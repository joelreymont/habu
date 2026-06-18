#!/bin/sh
# drive-habu.sh — Habu benchmark arms. The model writes a checked array word; we certify it
# (tools/check.sh), and on rejection feed the checker diagnostic back as the repair
# signal (up to N rounds). On certify, grade values via grade.sh (which builds the
# array in memory and runs the io-vectors). Emits one JSONL metrics row.
# Usage: drive-habu.sh <id> <name> <sig> <spec> <conv> <vectors> <a|lib|stdlib|skeleton> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
ID=$1 NAME=$2 SIG=$3 SPEC=$4 CONV=$5 VEC=$6 ARM=$7 MAXR=${8:-5}
model_init
case "$ARM" in
  a) PRE=$(cat bench/llm/habu-preamble.txt); LIB_MODULES=; SKELETON=0 ;;
  lib) PRE=$(cat bench/llm/habu-preamble-lib.txt); LIB_MODULES="errors array"; SKELETON=0 ;;
  stdlib) PRE=$(cat bench/llm/habu-preamble-stdlib.txt); LIB_MODULES="errors array string map fs argv test time date"; SKELETON=0 ;;
  skeleton) PRE=$(cat bench/llm/habu-preamble-skeleton.txt); LIB_MODULES=; SKELETON=1 ;;
  *) echo "drive-habu: unknown arm $ARM" >&2; exit 64 ;;
esac
T=$(mktemp -d "${TMPDIR:-/tmp}/dh.XXXXXX"); trap 'rm -rf "$T"' EXIT
printf 'prompt unavailable\n' > "$T/prompt.txt"
printf 'response unavailable\n' > "$T/resp.json"
printf '\\ no candidate extracted\n' > "$T/cand.f"
printf '\\ no final bundle\n' > "$T/bundle.f"
: > "$T/checker-diagnostics.txt"
: > "$T/repair-packet.json"
: > "$T/checker-stdout.txt"
: > "$T/checker-prose.txt"
: > "$T/test-output.txt"
REPAIR_TOOL=$T/repair-packet-tool.f
cat tools/argv.f tools/json.f tools/repair-packet.f > "$REPAIR_TOOL"
hb_test "$CONV" "$NAME" "$VEC" > "$T/vec.f"
hb_bench "$CONV" "$NAME" "$VEC" > "$T/bench.f"
cases=$(case_list "$VEC")
mode=$( [ "$CONV" = aa ] && echo "modify the array IN PLACE (write results back with !), returning nothing" || echo "return one integer" )
if [ "$SKELETON" = 1 ]; then
  TASK="Complete this checked definition skeleton:
  : ${NAME} ( ${SIG} ) {: arr:ptr len :}
    ... your body here ...
  ;
The input array pointer local is arr and the length local is len. ${SPEC}
For this task you must ${mode}. Output ONLY the definition body, without ': ${NAME}', the signature, locals, or ';'."
else
  TASK="Define the word ${NAME} with signature:
  : ${NAME} ( ${SIG} ) ...
The input is an integer array passed as (pointer, length). ${SPEC}
For this task you must ${mode}."
fi
extract() {
  sed 's/^```.*$//' "$1" | awk '
    /^[[:space:]]*:/ { s = 1 }
    s { print; if ($0 ~ /;/) exit }
  '
}
bundle() {
  if [ -n "$LIB_MODULES" ]; then
    tools/bundle-lib.sh -o "$T/prelude.f" $LIB_MODULES -- bench/llm/habu-array-lib.f
    cat "$T/prelude.f" "$T/cand.f" > "$T/bundle.f"
  else cp "$T/cand.f" "$T/bundle.f"; fi
}
extract_skeleton() {
  if grep -q '^[[:space:]]*:' "$1"; then
    extract "$1" > "$T/cand.f"
  else
    sed 's/^```.*$//' "$1" | awk 'NF { print }' > "$T/body.f"
    if [ -s "$T/body.f" ]; then
      {
        printf ': %s ( %s ) {: arr:ptr len :}\n' "$NAME" "$SIG"
        cat "$T/body.f"
        printf '\n;\n'
      } > "$T/cand.f"
    else
      printf '\\ no candidate extracted\n' > "$T/cand.f"
    fi
  fi
}
runtime_ms() {
  {
    cat "$T/bundle.f"; printf '\n'
    printf '0 set-check\nvariable AP\n'
    printf ': BENCH-ONCE ( -- )\n'
    cat "$T/bench.f"
    printf ';\n'
    printf ': BENCH-MEASURE ( -- n )\n'
    printf '  %s 0 ?do BENCH-ONCE loop\n' "$(bench_runtime_warmups)"
    printf '  mono-ns\n'
    printf '  %s 0 ?do BENCH-ONCE loop\n' "$(bench_runtime_repetitions)"
    printf '  mono-ns swap - 999999 + 1000000 /\n'
    printf ';\n'
    printf 'BENCH-MEASURE . cr\n'
  } > "$T/runtime.f"
  timeout 5 bin/hb < "$T/runtime.f" 2>/dev/null | bench_runtime_ms_from_output
}

round=0; feedback=""; outcome=reject; toks=0; t0=$(now_ms)
while [ "$round" -lt "$MAXR" ]; do
  round=$((round+1))
  prompt="${PRE}

${TASK}${feedback}"
  printf '%s' "$prompt" > "$T/prompt.txt"
  model_run "$prompt" "$T/resp.json" \
    || { printf 'model_run_failed\n' > "$T/resp.json"; outcome=error; break; }
  rt=$(node bench/llm/parse-resp.js "$T/resp.json" "$T/text.txt" "$MODEL_PARSER" "$MODEL_TOKEN_FIELDS"); toks=$((toks+rt))
  if [ "$SKELETON" = 1 ]; then
    extract_skeleton "$T/text.txt"
  else
    extract "$T/text.txt" > "$T/cand.f"
  fi
  [ -s "$T/cand.f" ] || printf '\\ no candidate extracted\n' > "$T/cand.f"
  if ! grep -q ';' "$T/cand.f"; then
    feedback="

You produced no valid definition. Output ONLY the habu definition."
    outcome=reject; continue
  fi
  bundle
  if tools/check.sh "$T/bundle.f" >"$T/checker-stdout.txt" 2>"$T/checker-prose.txt"; then
    outcome=$(sh bench/llm/grade.sh 5 "$T/bundle.f" "$T/vec.f")
    printf '%s\n' "$outcome" > "$T/test-output.txt"
    [ "$outcome" = pass ] && break
    feedback="

Your attempt:
$(cat "$T/cand.f")

It certified but FAILED the tests. It must satisfy (input -> expected):
${cases}
Fix the logic. Output ONLY the corrected definition."
  else
    tools/check.sh --json-errors --all-errors "$T/bundle.f" >"$T/checker-stdout.txt" 2>"$T/checker-diagnostics.txt" || true
    if [ ! -s "$T/checker-diagnostics.txt" ] && [ -s "$T/checker-stdout.txt" ]; then
      cp "$T/checker-stdout.txt" "$T/checker-diagnostics.txt"
    fi
    if ! bin/hb "$REPAIR_TOOL" "$T/checker-diagnostics.txt" > "$T/repair-packet.json"; then
      outcome=error
      break
    fi
    BENCH_DIAGNOSTIC_COUNT=$(sed -n 's/.*"diagnostic_count":\([0-9][0-9]*\).*/\1/p' "$T/repair-packet.json")
    [ -n "$BENCH_DIAGNOSTIC_COUNT" ] || BENCH_DIAGNOSTIC_COUNT=1
    cat "$T/repair-packet.json" > "$T/test-output.txt"
    feedback="

Your attempt:
$(cat "$T/cand.f")

The checker REJECTED it. Use this repair packet:
$(cat "$T/repair-packet.json")

Fix it so it certifies. Output ONLY the corrected definition."
    outcome=reject
  fi
done
wall=$(( $(now_ms) - t0 ))
rt_ms=null; rt_status=not_run
if [ "$outcome" = pass ]; then
  if rt=$(runtime_ms); then
    rt_ms=$rt; rt_status=ok
  else
    outcome=error; rt_status=error
  fi
fi
BENCH_PROMPT_FILE=$T/prompt.txt
BENCH_RAW_RESPONSE_FILE=$T/resp.json
BENCH_CANDIDATE_FILE=$T/cand.f
BENCH_CHECKER_DIAGNOSTICS_FILE=$T/checker-diagnostics.txt
BENCH_REPAIR_PACKET_FILE=$T/repair-packet.json
BENCH_TEST_OUTPUT_FILE=$T/test-output.txt
BENCH_FINAL_BUNDLE_FILE=$T/bundle.f
BENCH_SOURCE_FILE=$T/cand.f
emit_row "$ID" "$NAME" "$MODEL" "habu-$ARM" "$outcome" "$round" "$toks" "$wall" "$rt_ms" "$rt_status"
