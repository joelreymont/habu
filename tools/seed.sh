#!/bin/sh
# seed.sh — install a trusted native hb seed and rebuild from source.
set -e
cd "$(dirname "$0")/.."

usage() {
  echo "usage: tools/seed.sh /path/to/hb-seed"
  echo "optional: HABU_SEED_SHA256=<hex> tools/seed.sh /path/to/hb-seed"
  exit 64
}

[ "$#" -eq 1 ] || usage
SEED=$1
[ -f "$SEED" ] || { echo "seed: not a file: $SEED"; exit 66; }

if [ -n "${HABU_SEED_SHA256:-}" ]; then
  GOT=$(shasum -a 256 "$SEED" | awk '{print $1}')
  [ "$GOT" = "$HABU_SEED_SHA256" ] || {
    echo "seed: sha256 mismatch"
    echo "seed: expected $HABU_SEED_SHA256"
    echo "seed: got      $GOT"
    exit 65
  }
fi

mkdir -p bin
cp "$SEED" bin/hb
chmod +x bin/hb

if ! codesign -v bin/hb 2>/dev/null; then
  codesign -s - --force bin/hb 2>/dev/null || {
    echo "seed: codesign verification/signing failed"
    exit 69
  }
fi

OUT=$(printf '41 1 + . cr\n' | bin/hb 2>/dev/null) || {
  echo "seed: smoke program failed"
  exit 70
}
[ "$OUT" = "42" ] || {
  echo "seed: smoke program returned '$OUT', want 42"
  exit 70
}

./tools/build.sh
echo "seed OK: trusted seed rebuilt current bin/hb"
