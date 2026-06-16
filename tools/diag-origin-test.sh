#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-diag-origin.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/diag-origin.bundle.f
cat "$ROOT/tools/lint/lib.f" "$ROOT/tools/diag-origin.f" > "$BUNDLE"

run_diag() {
  "$ROOT/bin/hb" "$BUNDLE" "$1"
}

cat > "$T/input.f" <<'EOF'
\ : COMMENTED ;
s" : STRING ;"
: OK ( n -- n ) dup ;
( : PAREN ; )
: ;
EOF

run_diag "$T/input.f" > "$T/got.f"
cat > "$T/want.f" <<'EOF'
\ : COMMENTED ;
s" : STRING ;"

3 3 33 DIAG-ORIGIN!
: OK ( n -- n ) dup ;
( : PAREN ; )

5 3 69 DIAG-ORIGIN!
: ;
EOF
cmp "$T/got.f" "$T/want.f" || {
  echo "FAIL: diag-origin output drift"
  diff -u "$T/want.f" "$T/got.f" || true
  exit 1
}

echo "PASS: diag-origin fixtures"
