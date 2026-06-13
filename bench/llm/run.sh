#!/bin/sh
# run.sh — validate the benchmark reference answer key: every reference solution
# must typecheck (CHECK! => certified). This keeps the suite honest as the checker
# evolves. See PROTOCOL.md for running an actual LLM evaluation against tasks.md.
set -e
cd "$(dirname "$0")/../.."
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-/tmp/habu-gforth-cache}
G=${GFORTH:-$HOME/.local/bin/gforth}
[ -d "$XDG_CACHE_HOME/gforth" ] || "$G" -e 's" true" system bye' >/dev/null 2>&1
OUT=$("$G" bench/llm/validate.fs -e bye 2>/dev/null)
CERT=$(printf '%s' "$OUT" | tr -s ' ' '\n' | grep -c -- '-1' || true)
REJ=$(printf '%s'  "$OUT" | tr -s ' ' '\n' | grep -cx '0'  || true)
N=$(grep -c '^: ' bench/llm/solutions.f)
echo "habu LLM bench: $CERT/$N reference solutions certified, $REJ rejected"
{ [ "$CERT" = "$N" ] && [ "$REJ" = 0 ]; } || { echo "FAIL: answer key is not all-certified"; exit 1; }
echo "PASS: answer key valid"
