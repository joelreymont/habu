#!/bin/sh
# Focused black-box tests for tools/repl-lint.f.
set -eu

cd "$(dirname "$0")/.."
ROOT=$(pwd)
HB=${HABU_HB:-$ROOT/bin/hb}

if [ ! -x "$HB" ]; then
  echo "SKIP: $HB missing"
  exit 77
fi

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-repl-lint-test.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

mkdir -p "$T/src/habu"
cat > "$T/src/habu/stdin.f" <<'EOF'
: REPL-SRC s" src/habu/repl.f" ;
: STEP-SRC s" src/habu/stepper.f" ;
: DBG-SRC  s" src/habu/debug.f" ;
EOF
: > "$T/src/habu/stepper.f"
: > "$T/src/habu/debug.f"

run_lint() {
  (
    cd "$T"
    cat "$ROOT/tools/lint/lib.f" \
        "$ROOT/tools/repl-lint.f" | "$HB"
  )
}

cat > "$T/src/habu/repl.f" <<'EOF'
\ die in a line comment is allowed
: STRINGY s" die" drop ." bye" ;
: STACKY ( die in a stack comment ) 1 ;
EOF

out=$(run_lint)
if [ "$out" != "repl-lint: 0 finding(s)" ]; then
  echo "FAIL: comment/string fixture was not clean"
  printf '%s\n' "$out"
  exit 1
fi

cat > "$T/src/habu/repl.f" <<'EOF'
: BAD die ;
EOF

set +e
out=$(run_lint 2>&1)
rc=$?
set -e

if [ "$rc" -eq 0 ]; then
  echo "FAIL: code fixture did not fail"
  printf '%s\n' "$out"
  exit 1
fi
printf '%s\n' "$out" | grep -Fq 'FATAL-IN-REPL src/habu/repl.f:1: `die` exits the session'
printf '%s\n' "$out" | grep -Fq 'use `throw` (the REPL recovers); `die` is for build-time makers only'
printf '%s\n' "$out" | grep -Fq 'repl-lint: 1 finding(s)'

echo "repl-lint-test: ok"
