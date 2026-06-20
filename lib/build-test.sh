#!/bin/sh
set -e
cd "$(dirname "$0")/.."

if [ -n "${HABU_HB:-}" ]; then
  HB=$HABU_HB
elif [ -x bin/hb ]; then
  HB=bin/hb
else
  HB=/Users/joel/Work/habu/bin/hb
fi

[ -x "$HB" ] || { echo "build-test: bin/hb missing"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-build.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

SRC=$T/source.f
MISSING=$T/missing.f
ROOT=$T/root
ART=$ROOT/artifact.bin
BAD=$T/bad.f
UNCHECKABLE=$T/uncheckable.f
TOPDIE=$T/top-die.f
mkdir -p "$ROOT"
printf ': MAIN ( -- ) ;\n: INC ( i64 -- i64 ) 1 + ;\n' > "$SRC"
printf ': BAD ( i64 -- i64 ) 0= ;\n' > "$BAD"
printf ': UNCHECKABLE ( i64 -- i64 ) evaluate ;\n' > "$UNCHECKABLE"
printf 's" top-level code must not execute" 1 die\n: SAFE ( i64 -- i64 ) 1 + ;\n' > "$TOPDIE"

cat > "$T/cmd-ok" <<EOF
#!/bin/sh
printf artifact > "$ART"
EOF

cat > "$T/cmd-noart" <<'EOF'
#!/bin/sh
exit 0
EOF

cat > "$T/cmd-fail" <<'EOF'
#!/bin/sh
exit 7
EOF

chmod +x "$T/cmd-ok" "$T/cmd-noart" "$T/cmd-fail"

"$HB" --load lib/errors.f lib/string.f lib/fs.f lib/process.f lib/test.f lib/build.f \
  lib/build-test.f -- "$SRC" "$MISSING" "$ROOT" \
  "$T/cmd-ok" "$T/cmd-noart" "$T/cmd-fail" \
  "$BAD" "$UNCHECKABLE" "$TOPDIE" \
  > "$T/build-test.out" 2> "$T/build-test.err"
grep -F "build-test: ok" "$T/build-test.out" >/dev/null
grep -F "habu: in bad:" "$T/build-test.err" >/dev/null
grep -F "habu: in uncheckable:" "$T/build-test.err" >/dev/null
cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/build.f |
  ./tools/check.sh >/dev/null
