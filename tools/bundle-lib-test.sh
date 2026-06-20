#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

HB=${HABU_HB:-bin/hb}

fail() {
  echo "bundle-lib-test: $*" >&2
  exit 1
}

[ -x "$HB" ] || fail "missing executable $HB"

T=$(mktemp -d "${TMPDIR:-/tmp}/hb-bundle-lib.XXXXXX")
cleanup() {
  rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM

DRIVER=$T/driver.f
BUNDLE=$T/bundle.f
OUT=$T/out
ERR=$T/err

cat > "$DRIVER" <<'EOF'
\ bundle-lib smoke driver; loaded after lib/errors.f and lib/array.f.

100 constant BLT-FAIL

create BLT-DATA 3 , 1 , 4 ,

: BLT= ( n n -- ) {: got want :}
   got want <> IF s" bundle-lib-test: mismatch" BLT-FAIL die THEN ;

: BLT-SUM ( -- n )
   BLT-DATA 3 A-SUM ;

: BLT-ERROR-CODE ( -- n )
   E-A-BOUNDS ;

: BLT-MAIN ( -- )
   BLT-SUM 8 BLT=
   BLT-ERROR-CODE E-A-BOUNDS BLT=
   s" bundle-lib-test: ok" type cr ;

BLT-MAIN
EOF

if bin/hb tools/bundle-lib.f -o "$BUNDLE" errors missing-module -- "$DRIVER" 2>"$ERR"; then
  fail "missing module unexpectedly succeeded"
fi
grep -Fq "missing module" "$ERR" || fail "missing module error was not explicit"

if bin/hb tools/bundle-lib.f -o "$BUNDLE" errors array -- "$T/no-such-script.f" 2>"$ERR"; then
  fail "missing script unexpectedly succeeded"
fi
grep -Fq "missing script" "$ERR" || fail "missing script error was not explicit"

bin/hb tools/bundle-lib.f -o "$BUNDLE" errors array -- "$DRIVER"

grep -Fq "lib/errors.f" "$BUNDLE" || fail "bundle missing errors module marker"
grep -Fq "lib/array.f" "$BUNDLE" || fail "bundle missing array module marker"
grep -Fq "BLT-MAIN" "$BUNDLE" || fail "bundle missing driver"

tools/check.sh "$BUNDLE" > "$OUT"
grep -Fq "bundle-lib-test: ok" "$OUT" || fail "check.sh did not execute bundled script"

"$HB" "$BUNDLE" unused args > "$OUT"
grep -Fq "bundle-lib-test: ok" "$OUT" || fail "hb script mode did not run bundled source"

if [ -d bin ]; then
  public_bins=$(find bin -maxdepth 1 -type f -exec basename {} \; | sort | tr '\n' ' ')
  [ "$public_bins" = "hb " ] || fail "unexpected public binaries: $public_bins"
fi

echo "bundle-lib-test: ok"
