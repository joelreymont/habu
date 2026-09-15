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
    OS_PROCWATCH=src/os/macos/proc-watch.f
    OS_PROCCONTROL=src/os/macos/proc-control.f
    OS_IMAGE=src/os/macos/macho.f
    OS_SIGN=src/os/macos/sign2.f
    ;;
  linux-aarch64)
    OS_TARGET=src/os/linux/target.f
    OS_LAYOUT=src/os/linux/layout.f
    OS_SYS=src/os/linux/sys.f
    OS_PROCWATCH=src/os/linux/proc-watch.f
    OS_PROCCONTROL=src/os/linux/proc-control.f
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

# The compiler's load order after the checker core. Up to lower-cert-seal.f it
# is the engine prefix's own order (bootstrap/cg/forth.fs PFX-LOAD-CORE-FILES):
# the boot-hide prologue hides the startup prefix load's dictionary so this
# second load owns every word, so a file the prologue hides and this list does
# not carry is simply gone, and the order has to satisfy the same dependencies
# the prefix does. In particular the declaration participants
# (generated-declaration-dictionary.f and -protection.f, which seals their
# registration) precede every generated declaration, the first of which is
# lib/adt/option.f's ENUM, reached through src/habu/habu2.f's `require
# lib/fmt.f`. The boot-stdlib rows the prefix loads (PFX-LOAD-STDLIB-FILES)
# follow the seals as in the prefix: every `DYNAMIC-BUFFER NAME n` declaration
# generates an accessor that calls DYNAMIC-STORAGE:RESERVE, and the first one
# is in src/habu/primitive-registry.f, which src/habu/habu1.f requires from
# disk. The compiler sources come after, as they do in a native build.
SRC_COMMON=(
  src/core/roles.f
  src/core/bytes.f
  "$OS_TARGET"
  src/arch/arm64/asm.f
  src/arch/arm64/icode.f
  src/arch/arm64/mnem.f
  "$OS_LAYOUT"
  "$OS_SYS"
  src/habu/stack-abi.f
  src/habu/layout.f
  src/os/env-base.f
  src/core/include.f
  src/os/script-argv.f
  src/core/enums.f
  src/core/sha256.f
  src/core/type-family-sha.f
  src/core/combinators.f
  src/habu/code-span.f
  src/habu/xref.f
  src/core/generated-declaration-dictionary.f
  src/core/generated-declaration-protection.f
  src/core/layout-buffer-seal.f
  src/core/lower-cert-seal.f
  lib/prelude.f
  lib/errors.f
  src/core/dynamic-storage.f
  src/habu/treeshake.f
  src/habu/rt.f
  src/habu/crash.f
  src/os/image-bytes.f
  "$OS_IMAGE"
  "$OS_SIGN"
  "$OS_PROCWATCH"
  "$OS_PROCCONTROL"
  src/habu/habu1.f
  src/habu/prof.f
  src/habu/regalloc.f
  src/habu/jit.f
  src/habu/fdio.f
  src/habu/aot-decl.f
  src/habu/aot-ident.f
  src/habu/habu2.f
)

emit_boot_hide() {
  cat >> "$1" <<'EOF'
TRUSTED: BOOT-N>REC ( n -- ptr n ) ;
TRUSTED: BOOT-A>U8 ( ptr n -- ptr u8 ) ;
TRUSTED: BOOT-N>U8 ( n -- ptr u8 ) ;
TRUSTED: BOOT-USIG-END-PTR ( -- ptr a ) USIGS UEND @ + ;
TRUSTED: BOOT-UEND! ( n -- ) UEND ! ;
$0 constant BOOT-XREF-START-SLOT
$2 constant BOOT-XREF-FLAGS-SLOT
$3 constant BOOT-XREF-NAME-SLOT
: BOOT-XREF-REC ( n -- ptr n )
   DREC * dbase@ + BOOT-N>REC ;
: BOOT-XREF-CELL@ ( ptr n n -- n )
   cells + @ ;
: BOOT-XREF-PTR@ ( ptr n n -- ptr u8 )
   BOOT-XREF-CELL@ BOOT-N>U8 ;
: BOOT-XREF-START ( ptr n -- n )
   BOOT-XREF-START-SLOT BOOT-XREF-CELL@ ;
: BOOT-XREF-FLAGS ( ptr n -- n )
   BOOT-XREF-FLAGS-SLOT BOOT-XREF-CELL@ ;
: BOOT-XREF-NAME-LEN ( ptr n -- n )
   BOOT-XREF-FLAGS DNAME-LEN-MASK and ;
: BOOT-XREF-EXT? ( ptr n -- bool )
   BOOT-XREF-FLAGS DNAME-EXT and 0= 0= ;
: BOOT-XREF-INLINE-NAME ( ptr n -- ptr u8 )
   $18 + BOOT-A>U8 ;
: BOOT-XREF-NAME-A ( ptr n -- ptr u8 ) {: rec:ptr :}
   rec BOOT-XREF-EXT? if rec BOOT-XREF-NAME-SLOT BOOT-XREF-PTR@ exit then
   rec BOOT-XREF-INLINE-NAME ;
: BOOT-XREF-NAME$ ( ptr n -- ptr u8 n ) {: rec:ptr :}
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
: BOOT-XREF-MATCH? ( ptr n ptr u8 n -- bool ) {: rec:ptr name:ptr u:n :}
   rec BOOT-XREF-NAME$ name u BOOT-XREF-STR=CI ;
: BOOT-XREF-FIND-INDEX ( ptr u8 n -- n ) {: name:ptr u:n :}
   0
   begin dup ndict@ < while
      dup BOOT-XREF-REC name u BOOT-XREF-MATCH? if exit then
      1+
   repeat drop
   -1 ;
: BOOT-MIN-FOUND ( n n -- n ) {: a:n b:n :}
   a 0 < if b exit then
   b 0 < if a exit then
   a b < if a else b then ;
: BOOT-HIDE-DICT-FROM-EARLIEST ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   a u BOOT-XREF-FIND-INDEX  b v BOOT-XREF-FIND-INDEX  BOOT-MIN-FOUND
   dup 0 < if s" bootstrap: hide marker not found" 76 die then
   ndict! ;
: BOOT-USIGS-RESET ( -- )
   0 BOOT-UEND!
   0 BOOT-USIG-END-PTR ! ;
BOOT-USIGS-RESET
s" IMK-NDICT0" s" SEQ" BOOT-HIDE-DICT-FROM-EARLIEST
EOF
}

# The checker core ahead of SRC_COMMON, in load order. After the checker hook
# come the shared declaration-event transaction, the STRUCTURE constructor
# generator, the STRUCTURE declarer (which calls the generator, so it must come
# after it) and the ENUM declarer (a pure event-driven leaf).
SRC_CORE=(
  src/core/util.f
  src/core/cell.f
  src/core/pointer-storage.f
  src/core/engine-error.f
  src/core/exec-vector.f
  src/core/checker-fetch-abi.f
  src/core/checker-owner-abi.f
  src/core/checker.f
  src/core/engine-error-effects.f
  src/core/lower-cert-base.f
  src/core/type-schema.f
  src/core/type-family.f
  src/core/render.f
  src/core/sumtype.f
  src/core/layout-buffer.f
  src/core/layout-valid.f
  src/core/check-hook.f
  src/core/cell-effects.f
  src/core/declaration-transaction.f
  src/core/generated-declaration.f
  src/core/decl-event.f
  src/core/structure-make.f
  src/core/structure-decl.f
  src/core/enum-decl.f
  src/core/structures.f
)

# One `provided` row per file this text inlines. The second load defines its
# own src/core/include.f, whose registry starts empty, so without the rows a
# `require` met later in this text reads a file whose definitions the text
# already carries and dies on the duplicate: src/habu/rt.f requires
# src/habu/stack-abi.f, and src/habu/primitive-registry.f, which src/habu/habu1.f
# loads from disk, requires src/core/layout-buffer.f. The rows name every
# inlined file, including the ones still to come, because a row only settles
# what `require` skips; the definitions arrive in the order below regardless.
emit_provided() {
  local out="$1"
  shift
  local f
  for f in "$@"; do
    printf 's" %s" provided\n' "$f" >> "$out"
  done
}

emit_src() {
  local out="$1"
  local driver="$2"
  local tail=(src/habu/driver-io.f)
  if [[ "$driver" == "src/habu/stdin.f" ]]; then
    tail+=(src/habu/aot-arm.f src/habu/aot-capture.f src/habu/aot-file.f)
  fi
  tail+=("$driver")
  : > "$out"
  # Every engine built from this file re-reads the boot prefix from disk when it
  # starts, and then interprets this file, which loads the whole prefix a second
  # time. The second load must not inherit the first load's words. If it does,
  # `trust` and `checker-defer` from the startup load are still resolvable while
  # this file is being read, so a `defer NAME` declared before this file's own
  # `: TRUST` registers its effect row and its defer row into the startup load's
  # checker -- the one this file is in the middle of replacing. Those rows are
  # then invisible, the pending pre-trust defer table stays empty, and
  # DRAIN-PRETRUST has nothing to replay, so a later checked `is NAME` on that
  # defer cannot certify.
  #
  # The prologue below hides the startup load's dictionary and clears its
  # recorded effects, which is what makes the second load capture its pre-trust
  # defers and replay them into the checker that is actually live. It used to be
  # emitted only for the stage builds; the recovery seed that Gforth compiles
  # into hb-stage0 went without it, so the whole no-binary recovery path died at
  # src/habu/xref.f INSTALL with `hook: non-certified definition: install at
  # 'is'` and exit 70. There is one compiler source, so there is one prologue:
  # every consumer of this function gets it.
  emit_boot_hide "$out"
  printf "0 set-check\n" >> "$out"
  local f
  for f in "${SRC_CORE[@]}"; do
    cat "$f" >> "$out"
    printf '\n' >> "$out"
  done
  printf "LOWER-CERT-HOOK:INSTALL\n" >> "$out"
  for f in "${SRC_COMMON[@]}"; do
    cat "$f" >> "$out"
    printf '\n' >> "$out"
    if [[ "$f" == "src/core/include.f" ]]; then
      emit_provided "$out" "${SRC_CORE[@]}" "${SRC_COMMON[@]}" "${tail[@]}"
    fi
  done
  for f in "${tail[@]}"; do
    cat "$f" >> "$out"
    printf '\n' >> "$out"
  done
}

bootstrap_wide_gate() {
  "$GF" test/bootstrap-engine-stack.fs
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

# PERSISTED-PTR-VARIABLE is the boot prefix's only caller of the engine primitive
# `ptr-cell-mark`, so a stage0 generator that never registers it takes down every
# stage0 build at the prefix, naming only the bare token. This gate builds and runs
# a seed that uses the definer, so the omission -- or a seed word that consumes the
# wrong depth -- is named as itself.
bootstrap_ptr_cell_mark_gate() {
  "$GF" test/bootstrap-ptr-cell-mark.fs
}

bootstrap_ptr_cell_mark_gate

# An undefined word inside `evaluate` has to be a catchable rc-70 throw, not a
# rollback that returns and lets a handlerless caller keep interpreting.
# src/core/layout-buffer.f evaluates generated accessors and never reads
# EVALERR-CELL, so the fail-open form cost a SIGSEGV far from the offending token.
bootstrap_eval_undef_gate() {
  local src="test/bootstrap-eval-undef-src.f"
  local bin="$T/bootstrap-eval-undef"
  local out="$T/bootstrap-eval-undef.out"
  local err="$T/bootstrap-eval-undef.err"
  local marker=""
  local rc=0

  "$GF" -e "require $ROOT/test/nf.fs s\" $ROOT/$src\" slurp-file s\" $bin\" FORTH-EXE bye"
  set +e
  "$bin" >"$out" 2>"$err"
  rc=$?
  set -e
  if ! IFS= read -r marker < "$out"; then
    marker=""
  fi
  if [[ "$rc" -ne 70 || "$marker" != "BOOTSTRAP-EVAL-UNDEF-ARMED" ]]; then
    printf 'bootstrap eval undef: expected armed rc=70; got rc=%s marker=%s\n' "$rc" "$marker" >&2
    exit 75
  fi
  if grep -q 'BOOTSTRAP-EVAL-UNDEF-LEAKED' "$out"; then
    printf 'bootstrap eval undef: interpretation continued past the failed evaluate\n' >&2
    exit 75
  fi
}

bootstrap_eval_undef_gate

# The recovery engine publishes a created word's effect from its definer: `-- ptr a`
# for `create` and `variable`, `-- a` for `constant`, and the declared created
# effect for `create ... does>`. Every row goes through `trust-raw`, so its type
# variables are raw and cannot bind a nominal family. Each fixture below arms the
# marker and then offers one definition the checker must refuse; a fixture that
# certifies means the row is missing, wrong, or unsealed.
bootstrap_created_gate() {
  local src bin out err marker rc
  for src in bootstrap-created-effect bootstrap-created-raw \
             bootstrap-created-const bootstrap-created-does; do
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
    if [[ "$rc" -ne 70 || "$marker" != "BOOTSTRAP-CREATED-ARMED" ]]; then
      printf '%s: expected armed created rejection rc=70; got rc=%s marker=%s\n' "$src" "$rc" "$marker" >&2
      exit 75
    fi
  done
}

bootstrap_created_gate

bootstrap_preflight_recovery_gate() {
  local src="test/compile-preflight-recovery.f"
  local bin="$T/bootstrap-preflight-recovery"
  local out="$T/bootstrap-preflight-recovery.out"
  local err="$T/bootstrap-preflight-recovery.err"
  local marker=""
  local diag=""
  local rc=0

  "$GF" -e "require $ROOT/test/nf.fs s\" $ROOT/$src\" slurp-file s\" $bin\" FORTH-BUILD-EXE bye"
  set +e
  "$bin" >"$out" 2>"$err"
  rc=$?
  set -e
  if ! IFS= read -r marker < "$out"; then
    marker=""
  fi
  if ! IFS= read -r diag < "$err"; then
    diag=""
  fi
  if [[ "$rc" -ne 0 || "$marker" != "compile-preflight-recovery: ok" || "$diag" != "hb: compile preflight hook missing" ]]; then
    printf 'bootstrap preflight recovery: expected caught rc=0; got rc=%s marker=%s diagnostic=%s\n' "$rc" "$marker" "$diag" >&2
    exit 75
  fi
}

bootstrap_preflight_recovery_gate

# One emission serves both steps: Gforth compiles this text into hb-stage0, and
# hb-stage0 then compiles the same text into the first native stage.
emit_src "$T/stage2-src" src/habu/stage2.f
"$GF" -e "require $ROOT/test/nf.fs s\" $T/stage2-src\" slurp-file s\" $T/hb-stage0\" FORTH-BUILD-EXE bye"

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

emit_src "$T/stage2-src" src/habu/stdin.f
rm -f "$T/stage2-got" "$T/hb-stdin-got"
env HB_TMP="$T" "$T/hb-stage" -- "$T"
test -f "$T/stage2-got"
mv "$T/stage2-got" "$T/hb-stdin-mk"
chmod +x "$T/hb-stdin-mk"

env HB_TMP="$T" "$T/hb-stdin-mk"
test -f "$T/hb-stdin-got"
mv "$T/hb-stdin-got" "$T/hb-stdin"
chmod +x "$T/hb-stdin"

env HABU_UNDER_TEST="$T/hb-stdin" "$T/hb-stdin" --load test/engine-error-package.f
env HABU_UNDER_TEST="$T/hb-stdin" "$T/hb-stdin" --load test/catch-frame.f
env HABU_UNDER_TEST="$T/hb-stdin" "$T/hb-stdin" --load test/type-ctor-suite.f
env HABU_UNDER_TEST="$T/hb-stdin" "$T/hb-stdin" --load test/top-row-hook-test.f
env HABU_UNDER_TEST="$T/hb-stdin" "$T/hb-stdin" --load test/compile-preflight-recovery.f

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
