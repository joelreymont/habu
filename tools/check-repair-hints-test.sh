#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

T=$(mktemp -d "${TMPDIR:-/tmp}/hb-repair-hints.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

ASSERT=$T/gate-json-assert.f
cat tools/json.f tools/gate-json-assert.f > "$ASSERT"

check_hint() {
  name=$1
  class=$2
  body=$3
  src=$T/$name.f
  err=$T/$name.err
  printf '%s\n' "$body" > "$src"
  if ./tools/check.sh --json-errors "$src" >/dev/null 2>"$err"; then
    echo "FAIL: check-repair-hints accepted $name" >&2
    exit 1
  fi
  bin/hb "$ASSERT" diag-repair-class "$err" "$class"
}

check_hint remove-producer remove_producer \
  ': DIAG-REMOVE-PRODUCER ( i64 -- i64 ) dup ;'
check_hint add-producer add_producer \
  ': DIAG-ADD-PRODUCER ( i64 -- i64 ) drop ;'
check_hint fix-type fix_type \
  ': DIAG-FIX-TYPE ( i64 -- i64 ) 0= ;'
check_hint fix-return-stack fix_return_stack \
  ': DIAG-FIX-RSTACK ( i64 -- ) >r ;'
check_hint trusted-boundary trusted_boundary_required \
  ': DIAG-TRUSTED-BOUNDARY ( -- i64 ) evaluate ;'
check_hint signature-syntax fix_signature_syntax \
  ': DIAG-SIGNATURE-SYNTAX ( i64 ) 1 + ;'
check_hint rewrite-uncheckable rewrite_uncheckable \
  ': DIAG-REWRITE-UNCHECKABLE ( i64 -- i64 ) leave ;'

echo "PASS: check-repair-hints"
