#!/bin/sh
# grade-test.sh — teeth for grade.sh: prove all five outcomes are classified
# correctly, including the safety-critical ones (trap, timeout) that must NOT
# kill the harness. No LLM, fully deterministic.
set -e
cd "$(dirname "$0")/../.."
GRADE=bench/llm/grade.sh
T=$(mktemp -d "${TMPDIR:-/tmp}/grade-test.XXXXXX")
trap 'rm -rf "$T"' EXIT
fails=0
check() { # <name> <expected> <candidate-body> <vectors-body>
  printf '%s\n' "$3" > "$T/c.f"
  printf '%s\n' "$4" > "$T/v.f"
  got=$(sh "$GRADE" 3 "$T/c.f" "$T/v.f")
  if [ "$got" = "$2" ]; then
    echo "ok: $1 -> $got"
  else
    echo "FAIL: $1 -> got '$got', want '$2'"; fails=$((fails+1))
  fi
}

# certified + correct values
check pass    pass    ': SQ ( i64 -- i64 ) dup * ;'   '7 SQ 49 G='
# certified but wrong expected value
check fail    fail    ': SQ ( i64 -- i64 ) dup * ;'   '7 SQ 50 G='
# sig violation -> unpublished -> calling it exits 70
check reject  reject  ': SQ ( i64 -- i64 ) dup ;'     '7 SQ 49 G='
# certifies, but traps at runtime (/0) -> SIGABRT in the child, recorded not fatal
check trap    trap    ': DZ ( i64 -- i64 ) 0 / ;'     '7 DZ 0 G='
# certifies, but never terminates -> timeout(1) kills the child, recorded not fatal
check timeout timeout ': LP ( -- ) begin again ;'      'LP'

echo "----"
if [ "$fails" -eq 0 ]; then
  echo "PASS: grade.sh classifies pass/fail/reject/trap/timeout"
else
  echo "FAIL: grade.sh ($fails case(s))"; exit 1
fi
