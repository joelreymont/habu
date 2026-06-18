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

[ -x "$HB" ] || { echo "fs-mutate-test: bin/hb missing"; exit 69; }

T=$(mktemp -d "${TMPDIR:-/tmp}/habu-fs-mutate.XXXXXX")
cleanup() { rm -rf "$T"; }
trap cleanup EXIT HUP INT TERM

REMOVE=$T/remove.txt
RENAME_SRC=$T/rename-src.txt
RENAME_DST=$T/rename-dst.txt
CHMOD_PATH=$T/chmod.txt

printf 'delete-me' > "$REMOVE"
printf 'rename-me' > "$RENAME_SRC"
printf '#!/bin/sh\nexit 0\n' > "$CHMOD_PATH"

BUNDLE=$T/fs-mutate-test.f
cat lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f \
  lib/fs-mutate-test.f > "$BUNDLE"
"$HB" "$BUNDLE" "$REMOVE" "$RENAME_SRC" "$RENAME_DST" "$CHMOD_PATH" |
  grep -F "fs-mutate-test: ok" >/dev/null
cat lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f |
  ./tools/check.sh >/dev/null
