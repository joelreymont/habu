#!/bin/sh
# build.sh — rebuild the single installed binary, bin/hb, USING bin/hb.
#
# bin/hb is the checked native engine users run. Build-only compiler engines are
# temporary files under $HB_TMP. The checked Habu driver owns the source assembly,
# fixpoint loop, byte comparison, and artifact expectations; this shell wrapper
# owns private temp setup, command shims for OS operations not yet in the stdlib,
# and final installation of the already validated hb-new artifact.
set -e
cd "$(dirname "$0")/.."
CLEAN_T=0
if [ -n "${HB_TMP:-}" ]; then
  T=$HB_TMP
else
  T=$(mktemp -d "${TMPDIR:-/tmp}/hb-rebuild.XXXXXX")
  CLEAN_T=1
fi
mkdir -p "$T"
export HB_TMP=$T
cleanup() {
  [ "$CLEAN_T" = 0 ] || rm -rf "$T"
}
trap cleanup EXIT HUP INT TERM
[ -x bin/hb ] || { echo "no bin/hb — install a trusted seed with tools/seed.sh /path/to/hb"; exit 1; }

cat > "$T/build-bootstrap-stage" <<EOF
#!/bin/sh
set -eu
export HB_TMP="$T"
/bin/rm -f "$T/stage2-got" "$T/hb-stage"
bin/hb < src/habu/stage2.f
test -f "$T/stage2-got"
/bin/mv "$T/stage2-got" "$T/hb-stage"
/bin/chmod +x "$T/hb-stage"
EOF

cat > "$T/build-run-stage" <<EOF
#!/bin/sh
set -eu
export HB_TMP="$T"
/bin/rm -f "$T/stage2-got"
"$T/hb-stage"
test -f "$T/stage2-got"
EOF

cat > "$T/build-promote-stage" <<EOF
#!/bin/sh
set -eu
/bin/mv "$T/stage2-got" "$T/hb-stage"
/bin/chmod +x "$T/hb-stage"
EOF

cat > "$T/build-verify-stage" <<EOF
#!/bin/sh
set -eu
/usr/bin/codesign -v "$T/hb-stage"
EOF

cat > "$T/build-promote-stdin-maker" <<EOF
#!/bin/sh
set -eu
/bin/mv "$T/stage2-got" "$T/hb-stdin-mk"
/bin/chmod +x "$T/hb-stdin-mk"
EOF

cat > "$T/build-run-stdin-maker" <<EOF
#!/bin/sh
set -eu
export HB_TMP="$T"
/bin/rm -f "$T/hb-stdin-got"
"$T/hb-stdin-mk"
test -f "$T/hb-stdin-got"
EOF

cat > "$T/build-promote-stdin-engine" <<EOF
#!/bin/sh
set -eu
/bin/mv "$T/hb-stdin-got" "$T/hb-stdin"
/bin/chmod +x "$T/hb-stdin"
EOF

cat > "$T/build-verify-stdin" <<EOF
#!/bin/sh
set -eu
/usr/bin/codesign -v "$T/hb-stdin"
EOF

cat > "$T/build-run-snap" <<EOF
#!/bin/sh
set -eu
export HB_TMP="$T"
/bin/rm -f "$T/hb-snap0" "$T/hb-new"
"$T/hb-stdin" < "$T/hb-snap-src"
test -f "$T/hb-snap0"
EOF

cat > "$T/build-promote-snap" <<EOF
#!/bin/sh
set -eu
/bin/mv "$T/hb-snap0" "$T/hb-new"
/usr/bin/codesign -s - --force "$T/hb-new" 2>/dev/null
/bin/chmod +x "$T/hb-new"
EOF

/bin/chmod +x "$T"/build-*

./tools/bundle-lib.sh -o "$T/build-fixpoint.f" \
  errors string fs process build -- tools/build-fixpoint.f
HB_TMP=$T bin/hb "$T/build-fixpoint.f"

test -f "$T/hb-new" || { echo "build: checked hb image not produced"; exit 1; }
/bin/mv "$T/hb-new" bin/hb
/usr/bin/find bin -maxdepth 1 -type f ! -name hb -delete
echo "build OK: bin/hb (checked engine, tty REPL + stdin)"
