#!/bin/sh
# json-only-test.sh - exercise the Habu json-only wrapper tool.
set -e
cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}
[ -x "$HB" ] || { echo "json-only-test: $HB missing or not executable"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-json-only.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

TOOL=$T/json-only.f
cat tools/argv.f tools/json.f tools/json-only.f > "$TOOL"

run_case() {
  name=$1
  in=$2
  want_out=$3
  want_err=$4

  "$HB" "$TOOL" "$in" > "$T/$name.out" 2> "$T/$name.err"
  diff -u "$want_out" "$T/$name.out"
  diff -u "$want_err" "$T/$name.err"
}

printf '%s\n' \
  'prose before json' \
  '  {"a":1}  ' \
  '{bad' \
  '[1]' \
  '{"b":2}' > "$T/mixed.in"
printf '%s\n' '{"a":1}' '{"b":2}' > "$T/mixed.out.want"
: > "$T/empty.want"
run_case mixed "$T/mixed.in" "$T/mixed.out.want" "$T/empty.want"

printf '%s\n' '{bad' > "$T/bad.in"
run_case bad "$T/bad.in" "$T/empty.want" "$T/bad.in"

printf '%s\n' '[{"a":1}]' > "$T/array.in"
run_case array "$T/array.in" "$T/empty.want" "$T/array.in"

printf '%s\n' 'hello' 'world' > "$T/prose.in"
run_case prose "$T/prose.in" "$T/empty.want" "$T/prose.in"

: > "$T/zero.in"
run_case zero "$T/zero.in" "$T/empty.want" "$T/zero.in"

set +e
"$HB" "$TOOL" > "$T/noarg.out" 2> "$T/noarg.err"
rc=$?
set -e
[ "$rc" -eq 64 ] || { echo "json-only-test: no-arg rc $rc, want 64"; exit 1; }
grep -q 'usage: tools/json-only.f stderr-file' "$T/noarg.err"

echo "json-only-test: ok"
