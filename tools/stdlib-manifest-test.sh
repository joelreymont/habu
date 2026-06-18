#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}
MANIFEST=lib/std.manifest
DOC=docs/stdlib.md

fail() {
  echo "stdlib-manifest-test: $*" >&2
  exit 1
}

[ -f "$MANIFEST" ] || fail "missing $MANIFEST"
[ -f "$DOC" ] || fail "missing $DOC"
[ -x "$HB" ] || fail "missing executable $HB"

grep -Fq '## Handle Representation' "$DOC" || fail "missing Handle Representation section"
grep -Fq 'v1 memory-backed handles use `ptr u8 n`' "$DOC" || fail "missing ptr u8 n handle contract"
grep -Fq 'fixed-capacity map slot storage uses' "$DOC" || fail "missing ptr a n map storage contract"
grep -Fq 'Opaque `addr` values are boundary-only' "$DOC" || fail "missing addr boundary rule"

require_doc() {
  grep -Fq "$1" "$DOC" || fail "missing stdlib contract: $1"
}

require_doc '## LLM Surface'
require_doc '## String'
require_doc 'BYTE-COPY       ( ptr u8 ptr u8 n -- )'
require_doc 'SB-APPEND       ( ptr u8 n -- )'
require_doc '## Regex'
require_doc 'RX-COMPILE   ( ptr u8 n ptr u8 n -- n )'
require_doc '## Map'
require_doc 'MAP-CELLS           ( n -- n )'
require_doc 'MAP-SET     ( n ptr a n ptr u8 n -- )'
require_doc '## Files'
require_doc 'WALK-FILES   ( ptr u8 n [ ptr u8 n -- ] -- )'
require_doc 'depth-first'
require_doc 'per-depth recursion buffers'
require_doc '## Processes'
require_doc 'RUN-CAPTURE  ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )'
require_doc 'counted paths/commands'
require_doc 'FD_CLOEXEC'
require_doc 'rc in that order'
require_doc 'E-PROC-TRUNCATED'
require_doc 'E-PROC-TIMEOUT'
require_doc '## Argv'
require_doc 'ARGV-PARSE              ( -- )'
require_doc 'ARGV-EXPECT-POS-EXACT   ( n -- )'
require_doc 'ARGV-OUT$               ( -- ptr u8 n )'
require_doc '`--`'
require_doc 'ARGV-E-USAGE'
require_doc '## Test Property And Build Helpers'
require_doc 'T-RESET         ( -- )'
require_doc 'T=              ( n n -- )'
require_doc 'T-REPORT        ( -- )'
require_doc 'TTHROWS         ( a n -- )'
require_doc 'PROP-RUN-RESET  ( n n -- )'
require_doc 'PROP-SHRINK     ( [ -- bool ] -- )'
require_doc 'BUILD-STEP      ( ptr u8 n [ -- n ] -- )'
require_doc '`evaluate`'
require_doc 'source-string generation'
require_doc '## Build Shell Boundary'
require_doc 'Shell wrappers may only'
require_doc '`HB_TMP`'
require_doc 'Habu build helpers are responsible'

header=$(sed -n '1p' "$MANIFEST")
expected=$(printf 'schema_version\tmodule\tfile\tkind\tword\teffect\ttest\tdoc\towner\tstatus\tnotes')
[ "$header" = "$expected" ] || fail "unexpected manifest header"

awk '
BEGIN { FS = "\t"; ok = 1; modules = 0 }
NR == 1 { next }
NF != 11 {
  printf "%s:%d: expected 11 tab-separated columns, got %d\n", FILENAME, NR, NF > "/dev/stderr"
  ok = 0
  next
}
$1 != "1" {
  printf "%s:%d: schema_version must be 1\n", FILENAME, NR > "/dev/stderr"
  ok = 0
}
$2 !~ /^[a-z][a-z0-9-]*$/ {
  printf "%s:%d: invalid module name %s\n", FILENAME, NR, $2 > "/dev/stderr"
  ok = 0
}
$3 !~ /^lib\/[a-z][a-z0-9-]*\.f$/ {
  printf "%s:%d: file must be a stable lib/<module>.f path\n", FILENAME, NR > "/dev/stderr"
  ok = 0
}
$4 != "module" && $4 != "word" {
  printf "%s:%d: kind must be module or word\n", FILENAME, NR > "/dev/stderr"
  ok = 0
}
$8 == "" || $9 == "" || $11 == "" {
  printf "%s:%d: doc, owner, and notes are required\n", FILENAME, NR > "/dev/stderr"
  ok = 0
}
$10 !~ /^(planned|active|published)$/ {
  printf "%s:%d: status must be planned, active, or published\n", FILENAME, NR > "/dev/stderr"
  ok = 0
}
$4 == "module" {
  modules++
  if ($5 != "" || $6 != "") {
    printf "%s:%d: module rows must leave word and effect empty\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
}
$4 == "word" {
  if ($5 == "" || $6 == "") {
    printf "%s:%d: word rows require word and effect\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  if ($10 == "planned") {
    printf "%s:%d: word rows must be source-backed, not planned\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
}
END {
  if (modules == 0) {
    printf "%s: expected at least one module row\n", FILENAME > "/dev/stderr"
    ok = 0
  }
  exit ok ? 0 : 1
}
' "$MANIFEST"

T=$(mktemp -d "${TMPDIR:-/tmp}/hb-stdlib-manifest.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

awk 'BEGIN { FS = "\t" } NR > 1 { print $8 }' "$MANIFEST" | sort -u |
while IFS= read -r doc; do
  [ -f "$doc" ] || fail "doc path missing: $doc"
done

awk 'BEGIN { FS = "\t" } NR > 1 && $4 == "module" { print $3 }' "$MANIFEST" |
  sort -u > "$T/modules"

if ls lib/*.f >/dev/null 2>&1; then
  for file in lib/*.f; do
    case "$file" in
      lib/*-test.f) continue ;;
    esac
    grep -Fxq "$file" "$T/modules" || fail "missing module row for $file"
  done
fi

while IFS= read -r file; do
  [ -f "$file" ] && printf '%s\n' "$file"
done < "$T/modules" > "$T/existing-modules"

awk 'BEGIN { FS = "\t"; OFS = "\t" } NR > 1 && $4 == "word" { print $3, $5, $6 }' "$MANIFEST" |
  sort > "$T/manifest-words"

if [ -s "$T/existing-modules" ]; then
  script="$T/public-signatures.f"
  cat tools/lint/lib.f tools/public-signatures.f > "$script"
  # Manifest file paths are validated above, so word splitting is intentional.
  "$HB" "$script" $(cat "$T/existing-modules") > "$T/public.json"
  tr '{' '\n' < "$T/public.json" |
    sed -n 's/.*"word":"\([^"]*\)".*"file":"\([^"]*\)".*"signature":"\([^"]*\)".*/\2	\1	\3/p' |
    sort > "$T/public-words"
else
  : > "$T/public-words"
fi

diff -u "$T/public-words" "$T/manifest-words" || fail "public word rows drifted from checked signatures"

awk 'BEGIN { FS = "\t"; ok = 1 }
  $2 == "regex" && $4 == "module" && $11 !~ /ptr u8 n/ {
    printf "%s:%d: regex module notes must specify ptr u8 n handle representation\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  $2 == "map" && $4 == "module" && ($11 !~ /ptr a n/ || $11 !~ /ptr u8 n/) {
    printf "%s:%d: map module notes must specify ptr a n storage and ptr u8 n key representation\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  $2 == "fs" && $4 == "module" && $11 !~ /WALK-FILES/ {
    printf "%s:%d: fs module notes must mention WALK-FILES contract\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  $2 == "process" && $4 == "module" && ($11 !~ /RUN-CAPTURE/ || $11 !~ /FD_CLOEXEC/) {
    printf "%s:%d: process module notes must mention RUN-CAPTURE and FD_CLOEXEC\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  $2 == "test" && $4 == "module" && $11 !~ /checked assertions/ {
    printf "%s:%d: test module notes must mention checked assertions\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  $2 == "property" && $4 == "module" && ($11 !~ /PRNG/ || $11 !~ /evaluate boundary/) {
    printf "%s:%d: property module notes must mention PRNG and evaluate boundary\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  $2 == "build" && $4 == "module" && ($11 !~ /shell boundary/ || $11 !~ /Habu/) {
    printf "%s:%d: build module notes must mention shell boundary and Habu ownership\n", FILENAME, NR > "/dev/stderr"
    ok = 0
  }
  END { exit ok ? 0 : 1 }' "$MANIFEST" || fail "stdlib module notes missing"

echo "stdlib-manifest-test: ok"
