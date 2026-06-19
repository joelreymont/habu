#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

BUNDLE=${HABU_PARSE_RESP_BUNDLE:-${HB_TMP:-${TMPDIR:-/tmp}}/habu-llm-parse-resp.f}
TMP=$BUNDLE.tmp

if [ ! -f "$BUNDLE" ] ||
   [ tools/json.f -nt "$BUNDLE" ] ||
   [ tools/argv.f -nt "$BUNDLE" ] ||
   [ lib/errors.f -nt "$BUNDLE" ] ||
   [ lib/string.f -nt "$BUNDLE" ] ||
   [ lib/fs.f -nt "$BUNDLE" ] ||
   [ bench/llm/parse-resp.f -nt "$BUNDLE" ]; then
  cat lib/errors.f lib/string.f lib/fs.f tools/json.f tools/argv.f bench/llm/parse-resp.f > "$TMP"
  mv "$TMP" "$BUNDLE"
fi

exec bin/hb "$BUNDLE" "$@"
