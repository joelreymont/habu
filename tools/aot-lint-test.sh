#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-aot-lint.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/aot-lint.bundle.f
cat "$ROOT/tools/lint/lib.f" \
    "$ROOT/tools/lint/json-writer.f" \
    "$ROOT/tools/lint/source-lex.f" \
    "$ROOT/tools/argv.f" \
    "$ROOT/tools/aot-lint.f" > "$BUNDLE"

run_lint() {
  "$ROOT/bin/hb" "$BUNDLE" "$@"
}

cat > "$T/good.f" <<'EOF'
\ here in comment
s" here in string"
: MAIN ( -- ) 42 . CR ;
EOF
out=$(run_lint "$T/good.f")
[ -z "$out" ] || {
  echo "FAIL: aot-lint ok fixture emitted output: $out"
  exit 1
}

cat > "$T/bad.f" <<'EOF'
: MAIN ( -- ) here . CR ;
EOF
set +e
out=$(run_lint "$T/bad.f" 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: aot-lint accepted unsafe word"; exit 1; }
printf '%s\n' "$out" | grep -q 'E-AOT-UNSUPPORTED' || {
  echo "FAIL: aot-lint missing prose diagnostic"
  printf '%s\n' "$out"
  exit 1
}

set +e
out=$(run_lint --json --label '<stdin>' "$T/bad.f" 2>&1)
rc=$?
set -e
[ "$rc" -ne 0 ] || { echo "FAIL: aot-lint accepted unsafe word in JSON mode"; exit 1; }
printf '%s\n' "$out" | grep -q '"code":"E-AOT-UNSUPPORTED"' || {
  echo "FAIL: aot-lint missing JSON code"
  printf '%s\n' "$out"
  exit 1
}
printf '%s\n' "$out" | grep -q '"file":"<stdin>"' || {
  echo "FAIL: aot-lint missing JSON label"
  printf '%s\n' "$out"
  exit 1
}

echo "PASS: aot-lint fixtures"
