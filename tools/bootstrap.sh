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
  src/core/sha256.f
  src/core/type-family-sha.f
  src/core/combinators.f
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
  src/habu/engine-size.f
  src/habu/fdio.f
  src/habu/aot-decl.f
  src/habu/habu2.f
  src/habu/xref.f
  src/core/generated-declaration-dictionary.f
  src/core/generated-declaration-protection.f
  src/core/layout-buffer-seal.f
  src/core/lower-cert-seal.f
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

emit_decl_src() {
  # The shared declaration-event transaction, then the STRUCTURE constructor
  # generator, then the STRUCTURE declarer (which calls the generator, so it must
  # come after it), then the ENUM declarer (a pure event-driven leaf) -- all after
  # the checker hook.
  local out="$1"
  cat src/core/decl-event.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/structure-make.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/structure-decl.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/enum-decl.f >> "$out"
  printf '\n' >> "$out"
}

emit_src() {
  local out="$1"
  local driver="$2"
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
  cat src/core/util.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/cell.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/pointer-storage.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/engine-error.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/exec-vector.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/checker.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/engine-error-effects.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/lower-cert-base.f >> "$out"
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
  cat src/core/layout-valid.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/check-hook.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/cell-effects.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/pointer-storage-effects.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/declaration-transaction.f >> "$out"
  printf '\n' >> "$out"
  cat src/core/generated-declaration.f >> "$out"
  printf '\n' >> "$out"
  emit_decl_src "$out"
  cat src/core/structures.f >> "$out"
  printf '\n' >> "$out"
  printf "LOWER-CERT-HOOK:INSTALL\n" >> "$out"
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
    cat src/habu/aot-arm.f >> "$out"
    printf '\n' >> "$out"
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

# `using NAME` must import a package's public words in the recovery engine exactly
# as the native engine does: bare resolution while the import is open, nothing
# left visible after `;using`, and the two named failures (unknown package,
# ambiguous tail) exiting with their engine-error status. Each case compares the
# whole of stdout and the first stderr line, so a silent no-op import or a
# different diagnostic fails the recovery run.
bootstrap_using_case() {
  local name="$1"
  local want_rc="$2"
  local want_out="$3"
  local want_diag="$4"
  local bin="$T/$name"
  local out="$T/$name.out"
  local err="$T/$name.err"
  local diag=""
  local got=""
  local rc=0

  "$GF" -e "require $ROOT/test/nf.fs s\" $ROOT/test/$name-src.f\" slurp-file s\" $bin\" FORTH-EXE bye"
  set +e
  "$bin" >"$out" 2>"$err"
  rc=$?
  set -e
  # Compare whole streams: the undefined-word diagnostic has no trailing newline,
  # so a line-at-a-time read would silently see an empty string.
  got="$(cat "$out")"
  diag="$(cat "$err")"
  if [[ "$rc" -ne "$want_rc" || "$got" != "$want_out" || "$diag" != "$want_diag" ]]; then
    printf '%s: expected rc=%s stdout=%s diagnostic=%s; got rc=%s stdout=%s diagnostic=%s\n' \
      "$name" "$want_rc" "$want_out" "$want_diag" "$rc" "$got" "$diag" >&2
    exit 75
  fi
}

bootstrap_using_gate() {
  bootstrap_using_case bootstrap-using 0 \
    "$(printf '7\n7\n7\n3\n9\nBOOTSTRAP-USING-OK')" ""
  bootstrap_using_case bootstrap-using-unknown 91 \
    "BOOTSTRAP-USING-ARMED" "hb: using: unknown package: NOSUCH-PACKAGE"
  bootstrap_using_case bootstrap-using-ambiguous 94 \
    "BOOTSTRAP-USING-ARMED" "hb: ambiguous bare word resolves in multiple used packages: BUS-BOTH"
  # `;package` and the end of an evaluate frame both close the imports opened
  # inside them, so the bare name is undefined again. Stage0's undefined-word
  # diagnostic is the bare token (the native engine prefixes `E-UNDEFINED: `).
  bootstrap_using_case bootstrap-using-scope 70 \
    "BOOTSTRAP-USING-ARMED" "BUS-VALUE"
  # The engine hands the package name to the checker's CHECKER-USING, so the
  # checker resolves the same used publics as the engine once a recovery build
  # compiles src/core/checker.f.
  bootstrap_using_case bootstrap-using-checker-hook 0 \
    "$(printf 'checker-using: BUS-A\n7\nBOOTSTRAP-USING-HOOK-OK')" ""
}

bootstrap_using_gate

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
