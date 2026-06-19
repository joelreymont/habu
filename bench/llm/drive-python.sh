#!/bin/sh
# drive-python.sh - Python arm (array algorithms). f(a) takes a list; returns
# an integer (conv as) or a new list (conv aa). Repair on syntax/runtime/test
# failures. Uses only the local Python standard library at execution time.
# Usage: drive-python.sh <id> <name> <sig> <spec> <conv> <vectors> [maxr]
set -e
cd "$(dirname "$0")/../.."
. bench/llm/lib.sh
ID=$1 NAME=$2 SIG=$3 SPEC=$4 CONV=$5 VEC=$6 MAXR=${7:-5}
model_init
PYTHON=${PYTHON:-python3}
calls=$(py_test "$CONV" "$VEC")
bench=$(py_bench "$VEC")
T=$(mktemp -d "${TMPDIR:-/tmp}/dpy.XXXXXX"); trap 'rm -rf "$T"' EXIT
printf 'prompt unavailable\n' > "$T/prompt.txt"
printf 'response unavailable\n' > "$T/resp.json"
printf '# no candidate extracted\n' > "$T/f.py"
printf '# no final bundle\n' > "$T/test.py"
: > "$T/checker-diagnostics.txt"
: > "$T/repair-packet.json"
: > "$T/test-output.txt"
TASK="Write a Python function with this exact signature:
  def f(a):
      ...
where a is a list of integers. It must return $(py_ret "$CONV"). Use only the
Python standard library. ${SPEC}
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
  extract "$T/text.txt" > "$T/f.py"
  [ -s "$T/f.py" ] || printf '# no candidate extracted\n' > "$T/f.py"
  {
    cat "$T/f.py"; printf '\n'
    printf 'import sys\n'
    printf 'def check(g, w, a):\n'
    printf '    if g != w:\n'
    printf '        print("FAIL f(" + a + ") = " + repr(g) + " expected " + repr(w), file=sys.stderr)\n'
    printf '        sys.exit(1)\n'
    printf '%s\n' "$calls"
    printf 'print("ALL-OK")\n'
  } > "$T/test.py"
  set +e; out=$(timeout 5 "$PYTHON" "$T/test.py" 2>&1); rc=$?; set -e
  printf '%s\n' "$out" > "$T/test-output.txt"
  if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -q ALL-OK; then outcome=pass; break
  elif [ "$rc" -eq 124 ]; then outcome=timeout
  else outcome=fail; fi
  feedback="

Your attempt:
$(cat "$T/f.py")

It FAILED:
$(printf '%s' "$out" | head -8)

Fix it. Output ONLY the corrected function."
done
wall=$(( $(now_ms) - t0 ))
rt_ms=null; rt_status=not_run
if [ "$outcome" = pass ]; then
  {
    cat "$T/f.py"; printf '\n'
    printf '%s\n' "$bench"
  } > "$T/runtime.py"
  if rt=$(timeout 5 "$PYTHON" "$T/runtime.py" 2>/dev/null | bench_runtime_ms_from_output); then
    rt_ms=$rt; rt_status=ok
  else
    outcome=error; rt_status=error
  fi
fi
BENCH_PROMPT_FILE=$T/prompt.txt
BENCH_RAW_RESPONSE_FILE=$T/resp.json
BENCH_CANDIDATE_FILE=$T/f.py
BENCH_CHECKER_DIAGNOSTICS_FILE=$T/checker-diagnostics.txt
BENCH_REPAIR_PACKET_FILE=$T/repair-packet.json
BENCH_TEST_OUTPUT_FILE=$T/test-output.txt
BENCH_FINAL_BUNDLE_FILE=$T/test.py
BENCH_SOURCE_FILE=$T/f.py
emit_row "$ID" "$NAME" "$MODEL" "python" "$outcome" "$round" "$toks" "$wall" "$rt_ms" "$rt_status"
