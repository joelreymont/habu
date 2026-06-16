#!/usr/bin/env sh
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
HB=${HABU_HB:-$ROOT/bin/hb}
T=$(mktemp -d "${TMPDIR:-/tmp}/habu-stale-status.XXXXXX")
trap 'rm -rf "$T"' EXIT

cat "$ROOT/tools/date.f" "$ROOT/tools/lint/lib.f" "$ROOT/tools/fs.f" "$ROOT/tools/stale-status-lint.f" > "$T/stale-status-lint.f"

cat > "$T/STATUS.md" <<'EOF'
# Status

Last verified: 2026-06-16
Certified: 979  Uncheckable: 0  Rejected: 0
EOF
cat > "$T/LESSONS.md" <<'EOF'
Historical 783/0/0 count is allowed here.
EOF
cat > "$T/README.md" <<'EOF'
No live count here.
EOF

( cd "$T" && STALE_STATUS_TODAY=2026-06-16 "$HB" "$T/stale-status-lint.f" < /dev/null ) > "$T/clean.out"
grep -Fq 'stale-status-lint: 0 finding(s)' "$T/clean.out"

sed 's/2026-06-16/2026-06-15/' "$T/STATUS.md" > "$T/STATUS.next"
mv "$T/STATUS.next" "$T/STATUS.md"
if ( cd "$T" && STALE_STATUS_TODAY=2026-06-16 "$HB" "$T/stale-status-lint.f" < /dev/null ) > "$T/stale-date.out" 2>&1; then
  echo "FAIL: stale-status accepted stale Last verified"
  exit 1
fi
grep -Fq 'Last verified is 2026-06-15, expected 2026-06-16' "$T/stale-date.out"

sed 's/2026-06-15/2026-06-16/' "$T/STATUS.md" > "$T/STATUS.next"
mv "$T/STATUS.next" "$T/STATUS.md"

sed 's/2026-06-16/2026-02-29/' "$T/STATUS.md" > "$T/STATUS.next"
mv "$T/STATUS.next" "$T/STATUS.md"
if ( cd "$T" && STALE_STATUS_TODAY=2026-06-16 "$HB" "$T/stale-status-lint.f" < /dev/null ) > "$T/bad-status-date.out" 2>&1; then
  echo "FAIL: stale-status accepted invalid Last verified"
  exit 1
fi
grep -Fq 'BAD-STATUS-DATE STATUS.md: Last verified invalid `2026-02-29`' "$T/bad-status-date.out"

sed 's/2026-02-29/2026-06-16/' "$T/STATUS.md" > "$T/STATUS.next"
mv "$T/STATUS.next" "$T/STATUS.md"
if ( cd "$T" && STALE_STATUS_TODAY=2026-02-29 "$HB" "$T/stale-status-lint.f" < /dev/null ) > "$T/bad-today.out" 2>&1; then
  echo "FAIL: stale-status accepted invalid STALE_STATUS_TODAY"
  exit 1
fi
grep -Fq 'BAD-TODAY STALE_STATUS_TODAY invalid `2026-02-29`' "$T/bad-today.out"

cat > "$T/README.md" <<'EOF'
This stale count says 890 certified in prose.
EOF
if ( cd "$T" && STALE_STATUS_TODAY=2026-06-16 "$HB" "$T/stale-status-lint.f" < /dev/null ) > "$T/count.out" 2>&1; then
  echo "FAIL: stale-status accepted count-shaped prose"
  exit 1
fi
grep -Fq 'STALE-STATUS README.md:1: count-shaped string' "$T/count.out"

cat > "$T/README.md" <<'EOF'
This stale count says 890/0/0 in prose.
EOF
if ( cd "$T" && STALE_STATUS_TODAY=2026-06-16 "$HB" "$T/stale-status-lint.f" < /dev/null ) > "$T/triple.out" 2>&1; then
  echo "FAIL: stale-status accepted count-shaped triple"
  exit 1
fi
grep -Fq 'STALE-STATUS README.md:1: count-shaped string' "$T/triple.out"

echo "PASS: stale-status-lint"
