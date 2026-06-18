#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
ROOT=$(pwd)
T=$(mktemp -d "${TMPDIR:-/tmp}/hb-trust-lint.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

BUNDLE=$T/trust-lint.bundle.f
cat "$ROOT/tools/date.f" "$ROOT/tools/lint/lib.f" "$ROOT/tools/fs.f" "$ROOT/tools/trust-lint.f" > "$BUNDLE"

write_manifest_header() {
  cat > "$1/TRUSTED.md" <<'EOF'
| Word | Effect | Reason | Tests | Site | Last audited |
|------|--------|--------|-------|------|--------------|
EOF
}

make_base() {
  d=$T/$1
  mkdir -p "$d/src"
  cat > "$d/src/trust.f" <<'EOF'
s" foo" s" n -- n" TRUST
EOF
  write_manifest_header "$d"
  cat >> "$d/TRUSTED.md" <<'EOF'
| foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-13 |
EOF
}

add_lib_trust() {
  d=$1
  mkdir -p "$d/lib"
  cat > "$d/lib/trust.f" <<'EOF'
s" lib-foo" s" -- n" TRUST
EOF
}

add_lib_trusted_def() {
  d=$1
  mkdir -p "$d/lib"
  cat > "$d/lib/trusted-def.f" <<'EOF'
TRUSTED: lib-trusted ( n -- n )
  dup ;
EOF
}

run_lint() {
  d=$1
  today=${2:-2026-06-16}
  ( cd "$d" && TRUST_LINT_TODAY=$today "$ROOT/bin/hb" < "$BUNDLE" )
}

expect_ok_counts() {
  d=$1
  sites=$2
  rows=$3
  set +e
  out=$(run_lint "$d")
  rc=$?
  set -e
  [ "$rc" -eq 0 ] && [ "$out" = "trust-lint: $sites TRUST site(s), $rows manifest row(s), 0 finding(s)" ] || {
    echo "FAIL: trust-lint ok fixture:"
    printf '%s\n' "$out"
    exit 1
  }
}

expect_ok() {
  expect_ok_counts "$1" 1 1
}

expect_bad() {
  d=$1
  code=$2
  today=${3:-2026-06-16}
  set +e
  out=$(run_lint "$d" "$today" 2>&1)
  rc=$?
  set -e
  [ "$rc" -ne 0 ] || {
    echo "FAIL: trust-lint accepted $code fixture"
    exit 1
  }
  printf '%s\n' "$out" | grep -q "$code" || {
    echo "FAIL: trust-lint missing $code diagnostic"
    printf '%s\n' "$out"
    exit 1
  }
}

expect_bad_contains() {
  d=$1
  code=$2
  needle=$3
  today=${4:-2026-06-16}
  set +e
  out=$(run_lint "$d" "$today" 2>&1)
  rc=$?
  set -e
  [ "$rc" -ne 0 ] || {
    echo "FAIL: trust-lint accepted $code fixture"
    exit 1
  }
  printf '%s\n' "$out" | grep -q "$code" || {
    echo "FAIL: trust-lint missing $code diagnostic"
    printf '%s\n' "$out"
    exit 1
  }
  printf '%s\n' "$out" | grep -qF "$needle" || {
    echo "FAIL: trust-lint missing diagnostic text: $needle"
    printf '%s\n' "$out"
    exit 1
  }
}

make_base good
expect_ok "$T/good"

make_base good-lib
add_lib_trust "$T/good-lib"
add_lib_trusted_def "$T/good-lib"
cat >> "$T/good-lib/TRUSTED.md" <<'EOF'
| lib-foo | `-- n` | fixture | `test/t-lib-fixture.fs` | lib/trust.f:1 | 2026-06-13 |
| lib-trusted | `n -- n` | fixture | `test/t-lib-fixture.fs` | lib/trusted-def.f:1 | 2026-06-13 |
EOF
expect_ok_counts "$T/good-lib" 3 3

make_base unmanifested-lib
add_lib_trust "$T/unmanifested-lib"
expect_bad_contains "$T/unmanifested-lib" UNMANIFESTED "lib/trust.f:1"

make_base unmanifested-trusted-def
add_lib_trusted_def "$T/unmanifested-trusted-def"
expect_bad_contains "$T/unmanifested-trusted-def" UNMANIFESTED "lib/trusted-def.f:1"

make_base stale-lib-row
cat >> "$T/stale-lib-row/TRUSTED.md" <<'EOF'
| lib-gone | `--` | fixture | `test/t-lib-fixture.fs` | lib/missing.f:1 | 2026-06-13 |
EOF
expect_bad_contains "$T/stale-lib-row" STALE-ROW "lib/missing.f:1"

make_base duplicate-src-lib
mkdir -p "$T/duplicate-src-lib/lib"
cat > "$T/duplicate-src-lib/lib/trust.f" <<'EOF'
s" foo" s" n -- n" TRUST
EOF
expect_bad_contains "$T/duplicate-src-lib" DUPLICATE-TRUST "lib/trust.f:1"

make_base duplicate-trust
cat >> "$T/duplicate-trust/src/trust.f" <<'EOF'
s" foo" s" n -- n" TRUST
EOF
expect_bad "$T/duplicate-trust" DUPLICATE-TRUST

make_base effect-drift
write_manifest_header "$T/effect-drift"
cat >> "$T/effect-drift/TRUSTED.md" <<'EOF'
| foo | `n --` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-13 |
EOF
expect_bad "$T/effect-drift" EFFECT-DRIFT

make_base untested
write_manifest_header "$T/untested"
cat >> "$T/untested/TRUSTED.md" <<'EOF'
| foo | `n -- n` | fixture | | src/trust.f:1 | 2026-06-13 |
EOF
expect_bad "$T/untested" UNTESTED

make_base bad-audit
write_manifest_header "$T/bad-audit"
cat >> "$T/bad-audit/TRUSTED.md" <<'EOF'
| foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | nope |
EOF
expect_bad "$T/bad-audit" BAD-AUDIT-DATE

make_base bad-calendar-audit
write_manifest_header "$T/bad-calendar-audit"
cat >> "$T/bad-calendar-audit/TRUSTED.md" <<'EOF'
| foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-02-29 |
EOF
expect_bad "$T/bad-calendar-audit" BAD-AUDIT-DATE

make_base bad-today
expect_bad "$T/bad-today" BAD-TODAY 2026-02-29

make_base future-audit
write_manifest_header "$T/future-audit"
cat >> "$T/future-audit/TRUSTED.md" <<'EOF'
| foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-17 |
EOF
expect_bad "$T/future-audit" FUTURE-AUDIT

make_base stale-audit
expect_bad "$T/stale-audit" STALE-AUDIT 2026-10-01

make_base stale-row
cat >> "$T/stale-row/TRUSTED.md" <<'EOF'
| bar | `--` | fixture | `test/t-fixture.fs` | src/trust.f:2 | 2026-06-13 |
EOF
expect_bad "$T/stale-row" STALE-ROW

make_base duplicate-row
cat >> "$T/duplicate-row/TRUSTED.md" <<'EOF'
| foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-13 |
EOF
expect_bad "$T/duplicate-row" DUPLICATE-ROW

echo "PASS: trust-lint fixtures"
