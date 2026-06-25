#!/usr/bin/env bash
set -euo pipefail

if [[ "${HABU_ALLOW_BOOTSTRAP:-}" != "1" ]]; then
  printf 'set HABU_ALLOW_BOOTSTRAP=1 to bootstrap bin/hb\n' >&2
  exit 64
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

GF="${GFORTH:-gforth}"
if [[ -z "${HABU_TARGET:-}" ]]; then
  case "$(uname -s)-$(uname -m)" in
    Darwin-arm64|Darwin-aarch64) HABU_TARGET=macos-aarch64 ;;
    Linux-aarch64|Linux-arm64) HABU_TARGET=linux-aarch64 ;;
    *)
      printf 'unsupported bootstrap host %s-%s; set HABU_TARGET explicitly\n' "$(uname -s)" "$(uname -m)" >&2
      exit 64
      ;;
  esac
fi
export HABU_TARGET

case "$HABU_TARGET" in
  macos-aarch64)
    OS_LAYOUT=src/os/macos/layout.f
    OS_SYS=src/os/macos/sys.f
    OS_IMAGE=src/os/macos/macho.f
    OS_SIGN=src/os/macos/sign2.f
    ;;
  linux-aarch64)
    OS_LAYOUT=src/os/linux/layout.f
    OS_SYS=src/os/linux/sys.f
    OS_IMAGE=src/os/linux/elf.f
    OS_SIGN=src/os/linux/sign.f
    ;;
  *)
    printf 'unsupported HABU_TARGET=%s\n' "$HABU_TARGET" >&2
    exit 64
    ;;
esac

PROBE="$(mktemp "${TMPDIR:-/tmp}/habu-gforth-probe.XXXXXX")"
printf ': f {: a :} a . cr ; 1 f bye\n' > "$PROBE"
set +e
PROBE_OUT="$("$GF" "$PROBE" 2>&1)"
PROBE_RC=$?
set -e
rm -f "$PROBE"
if [[ "$PROBE_RC" -ne 0 || ( "$PROBE_OUT" != $'1 \n' && "$PROBE_OUT" != "1 " ) ]]; then
  printf 'Gforth must support {: :} locals and print exactly "1"; got rc=%s output:\n%s\n' "$PROBE_RC" "$PROBE_OUT" >&2
  exit 69
fi

if [[ -n "${HB_TMP:-}" ]]; then
  T="$HB_TMP"
  mkdir -p "$T"
else
  T="$(mktemp -d "${TMPDIR:-/tmp}/habu-gforth.XXXXXX")"
fi

SRC_COMMON=(
  src/arch/arm64/asm.f
  src/arch/arm64/icode.f
  src/arch/arm64/mnem.f
  "$OS_LAYOUT"
  "$OS_SYS"
  src/core/sha256.f
  src/core/combinators.f
  src/habu/layout.f
  src/habu/treeshake.f
  src/habu/rt.f
  src/habu/crash.f
  "$OS_IMAGE"
  "$OS_SIGN"
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
    if [[ "$f" == "src/habu/treeshake.f" ]]; then
      printf "0 set-check\n" >> "$out"
    fi
    if [[ "$f" == "src/habu/habu2.f" ]]; then
      printf "' HOOK set-check\n" >> "$out"
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
if [[ "$HABU_TARGET" == "macos-aarch64" ]]; then
  codesign -s - --force "$T/hb-new" >/dev/null
fi
chmod +x "$T/hb-new"

mkdir -p bin "$T/native"
OLD_HB="$T/bin-hb-before-bootstrap"
HAD_HB=0
if [[ -e bin/hb ]]; then
  mv bin/hb "$OLD_HB"
  HAD_HB=1
fi
restore_hb_on_failure() {
  local rc=$?
  if [[ "$rc" -ne 0 ]]; then
    rm -f bin/hb
    if [[ "$HAD_HB" == "1" ]]; then
      mv "$OLD_HB" bin/hb
    fi
  fi
  exit "$rc"
}
trap restore_hb_on_failure EXIT
mv "$T/hb-new" bin/hb
env HB_TMP="$T/native" bin/hb --load \
  lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install
trap - EXIT
rm -f "$OLD_HB"

printf 'bootstrap OK: bin/hb\n'
