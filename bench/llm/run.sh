#!/bin/sh
# run.sh — validate the benchmark reference answer key, functional tests, and
# reference metric data. This keeps the suite honest as the checker evolves.
set -e
cd "$(dirname "$0")/../.."
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-/tmp/habu-gforth-cache}
G=${GFORTH:-$HOME/.local/bin/gforth}
[ -d "$XDG_CACHE_HOME/gforth" ] || "$G" -e 's" true" system bye' >/dev/null 2>&1
OUT=$("$G" bench/llm/validate.fs -e bye 2>/dev/null)
CERT=$(printf '%s' "$OUT" | tr -s ' ' '\n' | grep -c -- '-1' || true)
REJ=$(printf '%s'  "$OUT" | tr -s ' ' '\n' | grep -cx '0'  || true)
N=$(awk 'NR>1{n++} END{print n+0}' bench/llm/tasks.tsv)
DEFN=$(grep -c '^: ' bench/llm/solutions.f)
[ "$DEFN" = "$N" ] || { echo "FAIL: task/solution count mismatch ($N task(s), $DEFN definition(s))"; exit 1; }
echo "habu LLM bench: $CERT/$N reference solutions certified, $REJ rejected"
{ [ "$CERT" = "$N" ] && [ "$REJ" = 0 ]; } || { echo "FAIL: answer key is not all-certified"; exit 1; }
TEST_OUT=$("$G" bench/llm/functional.fs -e bye 2>/dev/null)
[ "$TEST_OUT" = "ok" ] || { echo "FAIL: reference functional tests (got: $TEST_OUT)"; exit 1; }
./bench/llm/validate-results.py
echo "PASS: answer key valid ($N/$N certified, $N/$N tests passed, metrics valid)"
