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

[ -x "$HB" ] || { echo "build-fixpoint-test: bin/hb missing"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-build-fixpoint.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

cat > "$T/build-bootstrap-stage" <<EOF
#!/bin/sh
set -eu
printf gen0 > "$T/hb-stage"
EOF

cat > "$T/build-run-stage" <<EOF
#!/bin/sh
set -eu
case "\$(cat "$T/hb-stage")" in
  gen0) printf gen1 > "$T/stage2-got" ;;
  gen1) printf gen1 > "$T/stage2-got" ;;
  *) exit 7 ;;
esac
EOF

cat > "$T/build-promote-stage" <<EOF
#!/bin/sh
set -eu
cp "$T/stage2-got" "$T/hb-stage"
EOF

cat > "$T/build-verify-stage" <<EOF
#!/bin/sh
set -eu
test -f "$T/hb-stage"
EOF

cat > "$T/build-promote-stdin-maker" <<EOF
#!/bin/sh
set -eu
cp "$T/stage2-got" "$T/hb-stdin-mk"
EOF

cat > "$T/build-run-stdin-maker" <<EOF
#!/bin/sh
set -eu
printf stdin > "$T/hb-stdin-got"
EOF

cat > "$T/build-promote-stdin-engine" <<EOF
#!/bin/sh
set -eu
cp "$T/hb-stdin-got" "$T/hb-stdin"
EOF

cat > "$T/build-verify-stdin" <<EOF
#!/bin/sh
set -eu
test -f "$T/hb-stdin"
EOF

cat > "$T/build-run-snap" <<EOF
#!/bin/sh
set -eu
test -f "$T/hb-snap-src"
printf snapshot > "$T/hb-snap0"
EOF

cat > "$T/build-promote-snap" <<EOF
#!/bin/sh
set -eu
cp "$T/hb-snap0" "$T/hb-new"
EOF

chmod +x "$T"/build-*

./tools/bundle-lib.sh -o "$T/build-fixpoint.f" \
  errors string fs process build -- tools/build-fixpoint.f

HB_TMP=$T "$HB" "$T/build-fixpoint.f" > "$T/build-fixpoint.out" 2> "$T/build-fixpoint.err"
grep -F "build OK: stage compiler fixpoint" "$T/build-fixpoint.out" >/dev/null
grep -F "build OK: hb-new validated" "$T/build-fixpoint.out" >/dev/null
test -f "$T/hb-new"
grep -F ": HOOK CHECK ; ' HOOK set-check" "$T/stage2-src" >/dev/null
grep -F "STDIN-OUT" "$T/stage2-src" >/dev/null
grep -F "SNAP-MAGIC" "$T/hb-snap-src" >/dev/null

sed '$d' tools/build-fixpoint.f > "$T/build-fixpoint-defs.f"
cat lib/errors.f lib/string.f lib/fs.f lib/process.f lib/build.f "$T/build-fixpoint-defs.f" |
  ./tools/check.sh >/dev/null

echo "PASS: build fixpoint driver"
