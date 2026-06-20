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
  echo "fs-test: $*" >&2
  exit 1
}

[ -x "$HB" ] || fail "missing executable $HB"
[ -f lib/errors.f ] || fail "missing lib/errors.f"
[ -f lib/string.f ] || fail "missing lib/string.f"
[ -f lib/fs.f ] || fail "missing lib/fs.f"

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-fs.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

ROOT=$T/root
IO=$T/io.txt
BIG=$T/big.txt
mkdir -p "$ROOT/alpha/aa-child"
mkdir -p "$ROOT/.git" "$ROOT/.jj" "$ROOT/.dots"
mkdir -p "$ROOT/alpha/.git" "$ROOT/alpha/aa-child/.dots"
: > "$ROOT/alpha/aa-child/deep.txt"
: > "$ROOT/alpha/zz-after.txt"
: > "$ROOT/beta.txt"
: > "$ROOT/.git/ignored.txt"
: > "$ROOT/.jj/ignored.txt"
: > "$ROOT/.dots/ignored.txt"
: > "$ROOT/alpha/.git/ignored.txt"
: > "$ROOT/alpha/aa-child/.dots/ignored.txt"
printf 'abcd' > "$BIG"

DEEP=$T/deep
mkdir -p "$DEEP"
p=$DEEP
i=0
while [ "$i" -lt 40 ]; do
  p=$p/d$i
  mkdir -p "$p"
  i=$((i + 1))
done
: > "$p/leaf.txt"

"$HB" --load lib/errors.f lib/string.f lib/fs.f lib/fs-test.f -- \
  "$ROOT" "$DEEP" "$IO" "$BIG"
