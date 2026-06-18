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

[ -x "$HB" ] || { echo "process-test: bin/hb missing"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-process.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

cat > "$T/capture-ok" <<'EOF'
#!/bin/sh
printf 'out'
printf 'err' >&2
exit 7
EOF

cat > "$T/capture-long" <<'EOF'
#!/bin/sh
printf 'abcdef'
EOF

cat > "$T/capture-sleep" <<'EOF'
#!/bin/sh
sleep 2
EOF

chmod +x "$T/capture-ok" "$T/capture-long" "$T/capture-sleep"

cat lib/errors.f lib/test.f lib/process.f lib/process-test.f > "$T/process-test.f"
"$HB" "$T/process-test.f" "$T/capture-ok" "$T/capture-long" "$T/capture-sleep" |
  grep -F "process-test: ok" >/dev/null
cat lib/errors.f lib/process.f | ./tools/check.sh >/dev/null
