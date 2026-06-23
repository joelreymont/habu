#!/usr/bin/env bash
set -euo pipefail

if [[ "${HABU_ALLOW_BOOTSTRAP:-}" != "1" ]]; then
  printf 'set HABU_ALLOW_BOOTSTRAP=1 to bootstrap bin/hb\n' >&2
  exit 64
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

GF="${GFORTH:-gforth}"
printf ': f {: a :} a . ; 1 f bye\n' | "$GF" >/dev/null

if [[ -n "${HB_TMP:-}" ]]; then
  T="$HB_TMP"
  mkdir -p "$T"
else
  T="$(mktemp -d "${TMPDIR:-/tmp}/habu-gforth.XXXXXX")"
fi

SRC_COMMON=(
  src/core/util.f
  src/core/checker.f
  src/core/render.f
  src/core/roles.f
  src/core/sha256.f
  src/core/combinators.f
  src/arch/arm64/asm.f
  src/arch/arm64/icode.f
  src/arch/arm64/mnem.f
  src/os/macos/sys.f
  src/os/macos/env.f
  src/habu/treeshake.f
  src/habu/rt.f
  src/habu/crash.f
  src/os/macos/macho.f
  src/os/macos/sign2.f
  src/habu/habu1.f
  src/habu/prof.f
  src/habu/regalloc.f
  src/habu/jit.f
  src/habu/habu2.f
)

emit_src() {
  local out="$1"
  local driver="$2"
  : > "$out"
  local f
  for f in "${SRC_COMMON[@]}"; do
    cat "$f" >> "$out"
    printf '\n' >> "$out"
    if [[ "$f" == "src/core/render.f" ]]; then
      printf ": HOOK CHECK ; ' HOOK set-check\n" >> "$out"
    fi
  done
  cat "$driver" >> "$out"
  printf '\n' >> "$out"
}

emit_src "$T/stage2-src" src/habu/stage2.f
"$GF" -e "require $ROOT/test/nf.fs s\" $T/stage2-src\" slurp-file s\" $T/hb-stage0\" FORTH-EXE bye"

env HB_TMP="$T" "$T/hb-stage0"
test -f "$T/stage2-got"
mv "$T/stage2-got" "$T/hb-stage"
chmod +x "$T/hb-stage"

found=0
for gen in 1 2 3 4; do
  rm -f "$T/stage2-got"
  env HB_TMP="$T" "$T/hb-stage"
  test -f "$T/stage2-got"
  if cmp -s "$T/hb-stage" "$T/stage2-got"; then
    found=1
    break
  fi
  mv "$T/stage2-got" "$T/hb-stage"
  chmod +x "$T/hb-stage"
done

if [[ "$found" != "1" ]]; then
  printf 'bin/hb bootstrap did not reach fixpoint\n' >&2
  exit 74
fi

emit_src "$T/stage2-src" src/habu/stdin.f
rm -f "$T/stage2-got" "$T/hb-stdin-got"
env HB_TMP="$T" "$T/hb-stage"
test -f "$T/stage2-got"
mv "$T/stage2-got" "$T/hb-stdin-mk"
chmod +x "$T/hb-stdin-mk"

env HB_TMP="$T" "$T/hb-stdin-mk"
test -f "$T/hb-stdin-got"
mv "$T/hb-stdin-got" "$T/hb-stdin"
chmod +x "$T/hb-stdin"

emit_src "$T/hb-snap-src" src/habu/snap.f
rm -f "$T/hb-snap0" "$T/hb-new"
env HB_TMP="$T" "$T/hb-stdin" < "$T/hb-snap-src"
test -f "$T/hb-snap0"
mv "$T/hb-snap0" "$T/hb-new"
codesign -s - --force "$T/hb-new" >/dev/null
chmod +x "$T/hb-new"

mkdir -p bin "$T/native"
mv "$T/hb-new" bin/hb
env HB_TMP="$T/native" bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/build.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install

printf 'bootstrap OK: bin/hb\n'
