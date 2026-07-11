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
    OS_TARGET=src/os/macos/target.f
    OS_LAYOUT=src/os/macos/layout.f
    OS_SYS=src/os/macos/sys.f
    OS_IMAGE=src/os/macos/macho.f
    OS_SIGN=src/os/macos/sign2.f
    ;;
  linux-aarch64)
    OS_TARGET=src/os/linux/target.f
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
  src/core/roles.f
  "$OS_TARGET"
  src/arch/arm64/asm.f
  src/arch/arm64/icode.f
  src/arch/arm64/mnem.f
  "$OS_LAYOUT"
  "$OS_SYS"
  src/habu/layout.f
  src/os/env-base.f
  src/os/script-argv.f
  src/core/enums.f
  src/core/exec-vector.f
  src/core/sha256.f
  src/core/type-family-sha.f
  src/core/combinators.f
  src/habu/treeshake.f
  src/habu/rt.f
  src/habu/crash.f
  src/os/image-bytes.f
  "$OS_IMAGE"
  "$OS_SIGN"
  src/habu/habu1.f
  src/habu/prof.f
  src/habu/regalloc.f
  src/habu/jit.f
  src/habu/habu2.f
  src/habu/xref.f
  src/core/layout-buffer-seal.f
)

emit_boot_hide() {
  cat >> "$1" <<'EOF'
TRUSTED: BOOT-N>REC ( n -- ptr a ) ;
TRUSTED: BOOT-A>U8 ( ptr a -- ptr u8 ) ;
TRUSTED: BOOT-N>U8 ( n -- ptr u8 ) ;
TRUSTED: BOOT-USIG-END-PTR ( -- ptr a ) USIGS UEND @ + ;
TRUSTED: BOOT-UEND! ( n -- ) UEND ! ;
$0 constant BOOT-XREF-START-SLOT
$2 constant BOOT-XREF-FLAGS-SLOT
$3 constant BOOT-XREF-NAME-SLOT
: BOOT-XREF-REC ( n -- ptr a )
   DREC * dbase@ + BOOT-N>REC ;
: BOOT-XREF-CELL@ ( ptr a n -- n )
   cells + @ ;
: BOOT-XREF-PTR@ ( ptr a n -- ptr u8 )
   BOOT-XREF-CELL@ BOOT-N>U8 ;
: BOOT-XREF-START ( ptr a -- n )
   BOOT-XREF-START-SLOT BOOT-XREF-CELL@ ;
: BOOT-XREF-FLAGS ( ptr a -- n )
   BOOT-XREF-FLAGS-SLOT BOOT-XREF-CELL@ ;
: BOOT-XREF-NAME-LEN ( ptr a -- n )
   BOOT-XREF-FLAGS DNAME-LEN-MASK and ;
: BOOT-XREF-EXT? ( ptr a -- bool )
   BOOT-XREF-FLAGS DNAME-EXT and 0= 0= ;
: BOOT-XREF-INLINE-NAME ( ptr a -- ptr u8 )
   $18 + BOOT-A>U8 ;
: BOOT-XREF-NAME-A ( ptr a -- ptr u8 ) {: rec:ptr :}
   rec BOOT-XREF-EXT? if rec BOOT-XREF-NAME-SLOT BOOT-XREF-PTR@ exit then
   rec BOOT-XREF-INLINE-NAME ;
: BOOT-XREF-NAME$ ( ptr a -- ptr u8 n ) {: rec:ptr :}
   rec BOOT-XREF-NAME-A
   rec BOOT-XREF-NAME-LEN ;
: BOOT-XREF-FOLD-C ( n -- n ) {: c:n :}
   c $41 < if c exit then
   c $5A > if c exit then
   c $20 or ;
: BOOT-XREF-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ BOOT-XREF-FOLD-C
      over b + c@ BOOT-XREF-FOLD-C <> if drop 0 0= 0= exit then
      1+
   repeat drop
   0 0= ;
: BOOT-XREF-MATCH? ( ptr a ptr u8 n -- bool ) {: rec:ptr name:ptr u:n :}
   rec BOOT-XREF-NAME$ name u BOOT-XREF-STR=CI ;
: BOOT-XREF-FIND-INDEX ( ptr u8 n -- n ) {: name:ptr u:n :}
   ndict@ 1-
   begin dup 0 >= while
      dup BOOT-XREF-REC name u BOOT-XREF-MATCH? if exit then
      1-
   repeat drop
   -1 ;
: BOOT-HIDE-DICT-FROM ( ptr u8 n -- )
   BOOT-XREF-FIND-INDEX dup 0 < if s" bootstrap: hide word not found" 76 die then
   ndict! ;
: BOOT-USIGS-RESET ( -- )
   0 BOOT-UEND!
   0 BOOT-USIG-END-PTR ! ;
BOOT-USIGS-RESET
s" SEQ" BOOT-HIDE-DICT-FROM
EOF
}

emit_src() {
  local out="$1"
  local driver="$2"
  local mode="${3:-seed}"
  : > "$out"
  if [[ "$mode" == "native" ]]; then
    emit_boot_hide "$out"
  fi
  printf "0 set-check\n" >> "$out"
  cat src/core/util.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/structures.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/checker.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/type-schema.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/type-family.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/render.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/sumtype.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/layout-buffer.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/check-hook.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/structures-effects.f >> "$out"
  printf '\n' >> "$out"
  printf "' HOOK set-check\n" >> "$out"
  local f
  for f in "${SRC_COMMON[@]}"; do
    cat "$f" >> "$out"
    printf '\n' >> "$out"
  done
  if [[ "$driver" == "src/habu/snap.f" ]]; then
    cat src/core/include.f >> "$out"
    printf '\n' >> "$out"
  fi
  cat src/habu/driver-io.f >> "$out"
  printf '\n' >> "$out"
  if [[ "$driver" == "src/habu/stdin.f" ]]; then
    cat src/habu/aot-capture.f >> "$out"
    printf '\n' >> "$out"
  fi
  cat "$driver" >> "$out"
  printf '\n' >> "$out"
}

bootstrap_wide_gate() {
  "$GF" test/bootstrap-wide-memory.fs

  local src bin out err marker rc
  for src in bootstrap-wide-interpret bootstrap-wide-tick; do
    bin="$T/$src"
    out="$T/$src.out"
    err="$T/$src.err"
    "$GF" -e "require $ROOT/test/nf.fs s\" $ROOT/test/$src-src.f\" slurp-file s\" $bin\" FORTH-EXE bye"
    set +e
    "$bin" >"$out" 2>"$err"
    rc=$?
    set -e
    marker=""
    if ! IFS= read -r marker < "$out"; then
      marker=""
    fi
    if [[ "$rc" -ne 70 || "$marker" != "BOOTSTRAP-WIDE-ARMED" ]]; then
      printf '%s: expected armed wide rejection rc=70; got rc=%s marker=%s\n' "$src" "$rc" "$marker" >&2
      exit 75
    fi
  done
}

bootstrap_wide_gate

emit_src "$T/stage2-src" src/habu/stage2.f
"$GF" -e "require $ROOT/test/nf.fs s\" $T/stage2-src\" slurp-file s\" $T/hb-stage0\" FORTH-EXE bye"

emit_src "$T/stage2-src" src/habu/stage2.f native
env HB_TMP="$T" "$T/hb-stage0" -- "$T"
test -f "$T/stage2-got"
mv "$T/stage2-got" "$T/hb-stage"
chmod +x "$T/hb-stage"

found=0
for gen in 1 2 3 4; do
  rm -f "$T/stage2-got"
  env HB_TMP="$T" "$T/hb-stage" -- "$T"
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

emit_src "$T/stage2-src" src/habu/stdin.f native
rm -f "$T/stage2-got" "$T/hb-stdin-got"
env HB_TMP="$T" "$T/hb-stage" -- "$T"
test -f "$T/stage2-got"
mv "$T/stage2-got" "$T/hb-stdin-mk"
chmod +x "$T/hb-stdin-mk"

env HB_TMP="$T" "$T/hb-stdin-mk"
test -f "$T/hb-stdin-got"
mv "$T/hb-stdin-got" "$T/hb-stdin"
chmod +x "$T/hb-stdin"

if [[ "${HABU_BOOTSTRAP_CHECK_ONLY:-}" == "1" ]]; then
  printf 'bootstrap check OK: %s/hb-stdin\n' "$T"
  exit 0
fi

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
mv "$T/hb-stdin" bin/hb
env HB_TMP="$T/native" bin/hb --load \
  lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install --force
trap - EXIT
rm -f "$OLD_HB"

printf 'bootstrap OK: bin/hb\n'
