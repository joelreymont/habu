#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-signature-lint.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/signature-lint.bundle.f
cat "$ROOT/tools/lint/lib.f" \
    "$ROOT/tools/lint/json-writer.f" \
    "$ROOT/tools/lint/source-lex.f" \
    "$ROOT/tools/argv.f" \
    "$ROOT/tools/signature-lint.f" > "$BUNDLE"

run_lint() {
  "$ROOT/bin/hb" "$BUNDLE" "$@"
}

expect_ok() {
  src=$1
  out=$(run_lint "$src")
  [ -z "$out" ] || {
    echo "FAIL: signature-lint ok fixture emitted output: $out"
    exit 1
  }
}

expect_bad() {
  code=$1
  shift
  set +e
  out=$(run_lint "$@" 2>&1)
  rc=$?
  set -e
  [ "$rc" -ne 0 ] || {
    echo "FAIL: signature-lint accepted $code fixture"
    exit 1
  }
  printf '%s\n' "$out" | grep -q "$code" || {
    echo "FAIL: signature-lint missing $code diagnostic"
    printf '%s\n' "$out"
    exit 1
  }
}

cat > "$T/good.f" <<'EOF'
: OK ( n -- n ) dup ;
\ : COMMENTED dup ;
s" : STRING ;"
( : PAREN dup ; )
EOF
expect_ok "$T/good.f"

cat > "$T/missing.f" <<'EOF'
: NOSIG dup ;
EOF
expect_bad E-MISSING-SIGNATURE "$T/missing.f"
expect_bad '"code":"E-MISSING-SIGNATURE"' --json --label '<stdin>' "$T/missing.f"

cat > "$T/optout.f" <<'EOF'
: X ( infer ) dup ;
EOF
expect_bad '"code":"E-UNVERIFIED-SIGNATURE"' --json "$T/optout.f"

cat > "$T/missing-name.f" <<'EOF'
: ( n -- n ) dup ;
EOF
expect_bad E-MISSING-NAME "$T/missing-name.f"

echo "PASS: signature-lint fixtures"
