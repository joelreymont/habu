#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

if [ -n "${HABU_HB:-}" ]; then
  HB=$HABU_HB
elif [ -x bin/hb ]; then
  HB=bin/hb
else
  HB=/Users/joel/Work/habu/bin/hb
fi

fail() {
  echo "examples-test: $*" >&2
  exit 1
}

require_file() {
  [ -f "$1" ] || fail "missing $1"
}

run_bundle() {
  name=$1
  libs=$2
  src=$3
  shift 3

  require_file "$src"
  bundle=$T/$name.f
  : > "$bundle"
  for lib in $libs; do
    require_file "$lib"
    cat "$lib" >> "$bundle"
    printf '\n' >> "$bundle"
  done
  cat "$src" >> "$bundle"

  out=$("$HB" "$bundle" "$@")
  [ "$out" = "test: ok" ] || fail "$src: unexpected output: $out"
  echo "examples-test: $name ok"
}

[ -x "$HB" ] || fail "missing executable $HB"

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-examples.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

ROOT=$T/files
mkdir -p "$ROOT/src" "$ROOT/docs" "$ROOT/build"
mkdir -p "$ROOT/.git" "$ROOT/.jj" "$ROOT/.dots"
: > "$ROOT/src/main.f"
: > "$ROOT/src/util.f"
: > "$ROOT/docs/readme.txt"
: > "$ROOT/build/app.bin"
: > "$ROOT/.git/ignored.f"
: > "$ROOT/.jj/ignored.txt"
: > "$ROOT/.dots/ignored.f"

run_bundle array "lib/errors.f lib/test.f lib/array.f" \
  examples/array.f
run_bundle string-regex "lib/errors.f lib/string.f lib/test.f lib/regex.f" \
  examples/string-regex.f
run_bundle file-map "lib/errors.f lib/string.f lib/test.f lib/fs.f lib/map.f" \
  examples/file-map.f "$ROOT"
run_bundle property-test "lib/errors.f lib/string.f lib/test.f lib/property.f" \
  examples/property-test.f
run_bundle build-script "lib/errors.f lib/string.f lib/test.f lib/fs.f lib/argv.f" \
  examples/build-script.f --json -o "$T/app.hb" examples/array.f

echo "examples-test: ok"
