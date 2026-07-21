#!/usr/bin/env bash
# fetch-gpt2-model.sh - fetch + hash-verify the REAL HuggingFace GPT-2 weights
# (openai-community/gpt2 model.safetensors, ~523 MiB, 160 F32 tensors). These are
# large regenerable reference data, so they are NEVER committed - this script is
# the checked, pinned-hash fetch path that the presence-gated real-artifact leg of
# maki/infer/safetensors-test.f loads (dot habu-infer-safetensors-loader-0b58e06a).
#
# Usage:  maki/examples/nanogpt/fetch-gpt2-model.sh [DEST_DIR]   (default: ./gpt2-model)
# Exit 0 iff the file downloads AND its sha256 matches the pinned value below.
set -euo pipefail

DEST="${1:-gpt2-model}"
URL="https://huggingface.co/openai-community/gpt2/resolve/main/model.safetensors"

# Pinned sha256 (openai-community/gpt2 model.safetensors; verified 2026-07-21).
MODEL_SHA="248dfc3911869ec493c76e65bf2fcf7f615828b0254c12b473182f0f81d3a707"

mkdir -p "$DEST"
OUT="$DEST/model.safetensors"

echo "fetching $URL"
curl -fsSL -o "$OUT" "$URL"

got="$(sha256sum "$OUT" | cut -d' ' -f1)"
if [ "$got" != "$MODEL_SHA" ]; then
  echo "HASH MISMATCH for model.safetensors:" >&2
  echo "  want $MODEL_SHA" >&2
  echo "  got  $got"       >&2
  exit 1
fi
echo "ok   model.safetensors  sha256 $got"
echo "verified GPT-2 weights in $DEST/"
