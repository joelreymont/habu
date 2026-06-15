#!/bin/sh
# Focused tests for tools/filemap-lint.f.
set -eu
cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}
[ -x "$HB" ] || { echo "filemap-lint-test: $HB missing or not executable"; exit 69; }

cat tools/lint/lib.f tools/filemap-lint.f | "$HB" > /tmp/habu-filemap-lint.out
grep -Fq 'filemap-lint:' /tmp/habu-filemap-lint.out
grep -Fq '0 finding(s)' /tmp/habu-filemap-lint.out

echo "filemap-lint-test: ok"
