#!/bin/sh
# run.sh — validate the benchmark reference answer key, functional tests, and
# reference metric data using the native Habu engine only.
set -e
cd "$(dirname "$0")/../.."
T=$(mktemp -d "${TMPDIR:-/tmp}/habu-llm.XXXXXX")
cleanup() {
  if command -v trash >/dev/null 2>&1; then
    trash "$T"
  else
    rm -r "$T"
  fi
}
trap cleanup EXIT HUP INT TERM
N=$(awk 'NR>1{n++} END{print n+0}' bench/llm/tasks.tsv)
DEFN=$(grep -c '^: ' bench/llm/solutions.f)
[ "$DEFN" = "$N" ] || { echo "FAIL: task/solution count mismatch ($N task(s), $DEFN definition(s))"; exit 1; }
[ -x bin/hb ] || ./tools/build.sh >/dev/null
./tools/check.sh bench/llm/solutions.f >"$T/check.out" 2>"$T/check.err" || {
  cat "$T/check.err"
  echo "FAIL: answer key is not all-certified"
  exit 1
}
echo "hb LLM bench: $N/$N reference solutions certified, 0 rejected"
TEST_OUT=$(cat bench/llm/solutions.f bench/llm/tests.f | bin/hb 2>"$T/tests.err")
[ "$TEST_OUT" = "ok" ] || { echo "FAIL: reference functional tests (got: $TEST_OUT)"; exit 1; }
VALIDATOR=$T/validate-results.f
cat tools/date.f tools/lint/lib.f tools/json.f tools/argv.f bench/llm/validate-results.f >"$VALIDATOR"
bin/hb "$VALIDATOR"
echo "PASS: answer key valid ($N/$N certified, $N/$N tests passed, metrics valid)"
