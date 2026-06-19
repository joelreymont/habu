#!/bin/sh
set -e
cd "$(dirname "$0")/../.."
HB=${HB:-bin/hb}
TMP=${HB_TMP:-${TMPDIR:-/tmp}}
BUNDLE="$TMP/habu-llm-report.f"
needs=0
[ -f "$BUNDLE" ] || needs=1
for f in lib/errors.f lib/string.f lib/fs.f tools/json.f tools/argv.f bench/llm/report.f; do
  [ "$f" -nt "$BUNDLE" ] && needs=1
done
if [ "$needs" -eq 1 ]; then
  {
    cat lib/errors.f
    cat lib/string.f
    cat lib/fs.f
    cat tools/json.f
    cat tools/argv.f
    cat bench/llm/report.f
  } > "$BUNDLE"
fi
exec "$HB" "$BUNDLE" "$@"
