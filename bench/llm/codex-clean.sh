#!/bin/sh
# Run Codex for benchmark calls with a minimal CODEX_HOME. The normal user home
# can contain plugins, skills, MCP config, and project state that inflate prompt
# tokens. This wrapper keeps auth/model cache visible while isolating benchmark
# runs from that ambient context.
set -eu

SRC=${CODEX_SOURCE_HOME:-$HOME/.codex}
HOME_DIR=${CODEX_BENCH_HOME:-${TMPDIR:-/tmp}/habu-codex-bench-home-${USER:-user}}

umask 077
mkdir -p "$HOME_DIR"
chmod 700 "$HOME_DIR"

if [ ! -e "$SRC/auth.json" ]; then
  echo "codex-clean: missing $SRC/auth.json" >&2
  exit 64
fi

if [ ! -e "$HOME_DIR/auth.json" ]; then
  ln -s "$SRC/auth.json" "$HOME_DIR/auth.json"
fi

for f in models_cache.json version.json; do
  if [ -e "$SRC/$f" ] && [ ! -e "$HOME_DIR/$f" ]; then
    ln -s "$SRC/$f" "$HOME_DIR/$f"
  fi
done

CODEX_HOME=$HOME_DIR exec codex "$@"
