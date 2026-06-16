#!/bin/sh
# grade.sh — grade one candidate program in an ISOLATED, TIMEOUT-BOUNDED child,
# so a runtime trap or infinite loop is RECORDED, not fatal to the harness. This
# is the Phase-A measurement spine (PLAN-AI.md §A6): adversarial LLM-authored code
# may trap (e.g. /0 -> SIGABRT) or hang (begin again), and the engine's in-process
# EVALERR recovers only undefined-word lookups — NOT hardware traps — so the run
# MUST happen in a child. Outcome is value-agnostic: no sentinel, no depth needed;
# correctness is decided by comparing against the task's io-vectors.
#
# Usage: grade.sh <timeout_secs> <candidate.f> <vectors.f>
#   candidate.f : the checked def(s) under test (typed defs auto-checked by bin/hb;
#                 a sig violation leaves the word unpublished)
#   vectors.f   : io-vector assertions, each of the form  <args> <WORD> <expected> G=
# Outcome on stdout (exactly one token): pass | fail | reject | trap | timeout | error
set -e
cd "$(dirname "$0")/../.."
NOCHECK=0
[ "${1:-}" = "--no-check" ] && { NOCHECK=1; shift; }   # arm (b): run unchecked
SECS=${1:?timeout secs}; CAND=${2:?candidate.f}; VEC=${3:?vectors.f}
[ -x bin/hb ] || { echo error; exit 1; }
[ -f "$CAND" ] && [ -f "$VEC" ] || { echo error; exit 1; }
T=$(mktemp -d "${TMPDIR:-/tmp}/grade.XXXXXX")
trap 'rm -rf "$T"' EXIT
# Assemble: candidate + grading harness (unchecked) + vectors + verdict.
# --no-check prepends `0 set-check` so the candidate runs even if it would NOT
# certify (habu arm b: judged purely by tests, like JS/Rust).
{
  [ "$NOCHECK" = 1 ] && printf '0 set-check\n'
  cat "$CAND"; printf '\n'
  printf '0 set-check\nvariable #BAD  0 #BAD !\nvariable AP  variable BP\n: G= ( got want ) <> if 1 #BAD +! then ;\n'
  printf ': GRADE-REPORT #BAD @ 0= if .\" GRADE-OK\" else .\" GRADE-FAIL\" then cr ;\n'
  cat "$VEC"; printf '\n'
  printf 'GRADE-REPORT\n'
} > "$T/prog.f"
set +e
out=$(timeout "$SECS" bin/hb < "$T/prog.f" 2>/dev/null)
rc=$?
set -e
if [ "$rc" -eq 0 ]; then
  case "$out" in *GRADE-OK*) echo pass ;; *) echo fail ;; esac
elif [ "$rc" -eq 70 ]; then
  echo reject                      # bad-sig word unpublished -> call exits 70
elif [ "$rc" -eq 124 ]; then
  echo timeout                     # timeout(1) killed the child
elif [ "$rc" -ge 128 ]; then
  echo trap                        # 128 + fatal signal (SIGABRT/SIGILL/SIGSEGV...)
else
  echo error
fi
