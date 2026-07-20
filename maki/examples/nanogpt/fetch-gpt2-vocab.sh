#!/usr/bin/env bash
# fetch-gpt2-vocab.sh - fetch + hash-verify the REAL GPT-2 byte-level BPE artifacts
# (dot habu-bpe-real-vocab-c973932a). These are the FULL 50257-token vocab (256 byte
# tokens + 50000 merges + <|endoftext|>): ~1.02 MB encoder.json + ~446 KB vocab.bpe.
# They are large regenerable reference data, so they are NEVER committed - this script
# is the checked, pinned-hash fetch path. maki/bpe-real-data.f commits only a small
# subset (byte-id table + the merges the parity fixtures fire), so the gate is hermetic.
#
# Usage:  maki/examples/nanogpt/fetch-gpt2-vocab.sh [DEST_DIR]   (default: ./gpt2-vocab)
# Exit 0 iff both files download AND both sha256 match the pinned values below.
set -euo pipefail

DEST="${1:-gpt2-vocab}"
BASE="https://openaipublic.blob.core.windows.net/gpt-2/models/117M"

# Pinned sha256 (identical across every GPT-2 model size; verified 2026-07-20).
ENC_SHA="196139668be63f3b5d6574427317ae82f612a97c5d1cdaf36ed2256dbf636783"
BPE_SHA="1ce1664773c50f3e0cc8842619a93edc4624525b728b188a9e0be33b7726adc5"

mkdir -p "$DEST"

fetch_verify() {
  local name="$1" want="$2" out="$DEST/$1"
  echo "fetching $BASE/$name"
  curl -fsSL -o "$out" "$BASE/$name"
  local got
  got="$(sha256sum "$out" | cut -d' ' -f1)"
  if [ "$got" != "$want" ]; then
    echo "HASH MISMATCH for $name:" >&2
    echo "  want $want" >&2
    echo "  got  $got"  >&2
    exit 1
  fi
  echo "ok   $name  sha256 $got"
}

fetch_verify encoder.json "$ENC_SHA"
fetch_verify vocab.bpe    "$BPE_SHA"
echo "verified GPT-2 vocab in $DEST/"
