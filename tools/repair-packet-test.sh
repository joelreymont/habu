#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}
[ -x "$HB" ] || { echo "repair-packet-test: $HB missing or not executable"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-repair-packet.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

TOOL=$T/repair-packet.f
ASSERT=$T/gate-json-assert.f
cat tools/argv.f tools/json.f tools/repair-packet.f > "$TOOL"
cat tools/json.f tools/gate-json-assert.f > "$ASSERT"

make_packet() {
  name=$1
  class=$2
  source=$3

  set +e
  printf '%s\n' "$source" | ./tools/check.sh --json-errors --all-errors >/dev/null 2>"$T/$name.err"
  rc=$?
  set -e
  [ "$rc" -ne 0 ] || { echo "repair-packet-test: checker accepted $name"; exit 1; }

  "$HB" "$TOOL" "$T/$name.err" > "$T/$name.packet"
  "$HB" "$ASSERT" repair-packet "$T/$name.packet" "$class"
  grep -Fq '"kind":"habu_repair_packet"' "$T/$name.packet"
  grep -Fq '"source_excerpt":' "$T/$name.packet"
  grep -Fq '"instruction":"Fix the definition so it certifies. Output only corrected Habu code."' "$T/$name.packet"
}

make_packet remove remove_producer ': DIAG-REMOVE ( i64 -- i64 ) dup ;'
make_packet add add_producer ': DIAG-ADD ( i64 -- i64 ) drop ;'
make_packet type fix_type ': DIAG-TYPE ( i64 -- i64 ) 0= ;'
make_packet rstack fix_return_stack ': DIAG-RSTACK ( i64 -- ) >r ;'

cat > "$T/two.f" <<'EOF'
: BAD1 ( i64 -- i64 ) dup ;
: BAD2 ( i64 -- ) >r ;
EOF
set +e
./tools/check.sh --json-errors --all-errors "$T/two.f" >/dev/null 2>"$T/two.err"
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "repair-packet-test: checker accepted two-error fixture"; exit 1; }
"$HB" "$TOOL" "$T/two.err" > "$T/two.packet"
"$HB" "$ASSERT" repair-packet "$T/two.packet" remove_producer
grep -Fq '"diagnostic_count":2' "$T/two.packet"

set +e
"$HB" "$TOOL" > "$T/noarg.out" 2>"$T/noarg.err"
rc=$?
set -e
[ "$rc" -eq 64 ] || { echo "repair-packet-test: no-arg rc $rc, want 64"; exit 1; }
grep -Fq 'usage: tools/repair-packet.f checker-jsonl.err' "$T/noarg.err"

echo "repair-packet-test: ok"
