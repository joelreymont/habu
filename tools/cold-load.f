\ cold-load.f - run a build on the tree it builds, not on the engine's copy.
\
\ The running hb answers a `require` of a startup file with the copy it baked,
\ so a tree that changed a startup file (renamed a package, say) would run its
\ build helpers against the old definitions. COLD-LOAD:RUN rewinds the running
\ hb to its first source record and loads the startup files of the tree holding
\ this file fresh, as Factor and CCL rebuild from their own sources: the owner
\ reset, `0 set-check`, `0 set-top-check` and `CORE-PREFIX:FIRST-RECORD
\ seed-ndict!` (src/habu/window-rewind.f's rewind), then the files and their
\ startup actions in the order tools/native-build-core.f LOAD-CHECKED-PREFIX
\ and LOAD-TARGET load them for the host, then src/habu/native-runtime.f,
\ which seals the checker itself, and REQUIRE-BOOT-FREEZE after it, the native
\ build's order. Each mirrors the other's order; one sequence is a later step.
\ A build entry runs it before its first `require` (tools/hb-build.f and the
\ support child tools/native-build-image.f PROGRAM! writes).
\
\ The text is staged in one buffer and evaluated as top-level text, because
\ `seed-ndict!` is a top-level boundary and because the rewind removes the
\ names of every definition above the first record, this file's own included,
\ while RUN is still running: the rewind keeps code and data, so RUN's body and
\ its buffer survive the names they lose (src/habu/prefix-rewind.f).
\
\ THE BOUNDARY. This file is compiled by the running hb before the reload, out
\ of the tree that may have renamed any internal word, so it names only the
\ language's documented vocabulary. Four engine cells and one engine word cross
\ that boundary; a tree that changes one needs an intermediate hb to build it:
\   - the owner reset's cells, TARGET-CELL and RESET-OFF below;
\   - the loading file's path cells, PATH-CELL and PATHLEN-CELL below, which
\     name this file while it loads and so give the tree's root;
\   - CORE-PREFIX:FIRST-RECORD (src/core/prefix-boundary.f), called by name.
require src/lib/errors.f
require src/habu/boot-files.f

package COLD-LOAD
private

$2800 constant PATH-CELL      \ = src/habu/layout.f SRCLOC:PATH-CELL
$2808 constant PATHLEN-CELL   \ = src/habu/layout.f SRCLOC:PATHLEN-CELL
$368 constant TARGET-CELL     \ = src/habu/layout.f TARGET-DECL-CELL
$70 constant RESET-OFF        \ = src/core/checker-owner-abi.f RESET-OFF

\ How much one read asks for; a file of any size takes as many reads as it needs.
65536 constant CHUNK

DYNAMIC-BUFFER TEXT u8
variable USED

\ The path of the file loading now, and a path cut to its directory.
: LOADING$ ( -- ptr u8 n )
   data-base PATH-CELL + 0 ptr-field @  data-base PATHLEN-CELL + @ ;

: PARENT ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u begin dup 0 > while
      1 - dup a + c@ 47 = if a swap exit then
   repeat drop
   E-FS-PATH throw ;

: KEEP ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ c, loop ;

\ The tree's root: this file is <root>/tools/cold-load.f.
LOADING$ PARENT PARENT dup constant ROOT-U
create ROOT KEEP align


: ROOM ( n -- ) USED @ + TEXT-RESERVE ;

: C ( n -- ) 1 ROOM USED @ TEXT c! 1 USED +! ;

: PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   u ROOM a USED @ TEXT u BYTE-COPY u USED +! ;

: LINE ( ptr u8 n -- ) PUT 10 C ;

: SAY ( ptr u8 n -- ) {: a:ptr u:n :} 2 a u write drop ;

\ <root>/<path> and a NUL, written past the staged text and not counted in it.
: PATH! ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT-U 1 + u + {: len:n :}
   len 1 + ROOM
   ROOT USED @ TEXT ROOT-U BYTE-COPY
   47 USED @ ROOT-U + TEXT c!
   a USED @ ROOT-U + 1 + TEXT u BYTE-COPY
   0 USED @ len + TEXT c!
   USED @ TEXT len ;

: FAIL ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   s" cold-load: cannot read " SAY a u PATH! SAY S\" \n" SAY
   code throw ;

\ Append a tree file's text and a newline.
: FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PATH! drop open-rd {: fd:n :}
   fd 0 < if a u E-FS-OPEN FAIL then
   begin
      CHUNK ROOM
      fd USED @ TEXT CHUNK read {: rd:n :}
      rd 0 < if fd close a u E-FS-IO FAIL then
      rd USED +!
      rd 0 >
   while repeat
   fd close
   10 C ;

\ The host's row kind, by the test src/habu/habu2.f PFX-LOAD? makes.
: HOST-KIND ( -- n )
   HB-TARGET-LINUX? if BOOT-FILES:LINUX exit then
   HB-TARGET-MACOS? if BOOT-FILES:MACOS exit then
   HB-TARGET-LINUX-X86-64? if BOOT-FILES:X64 exit then
   s" cold-load: unknown target" 76 die ;

: TARGET-ROW ( n n ptr u8 n -- ) {: idx:n kind:n a:ptr u:n :}
   kind HOST-KIND = if a u FILE then ;

\ s\" <root>" SOURCE-ROOT:ENGINE!, the root escaped for S\".
: ENGINE-ROOT ( -- )
   s" S" PUT 92 C 34 C 32 C
   ROOT-U 0 ?do
      ROOT i + c@ {: b:n :}
      b 34 = b 92 = or if 92 C then
      b C
   loop
   34 C s"  SOURCE-ROOT:ENGINE!" LINE ;

\ The order of tools/native-build-core.f LOAD-CHECKED-PREFIX and LOAD-TARGET.
: STAGE ( -- )
   s" 0 set-check" LINE
   s" 0 set-top-check" LINE
   s" CORE-PREFIX:FIRST-RECORD seed-ndict!" LINE
   s" src/core/util.f" FILE
   s" src/core/cell.f" FILE
   s" src/core/pointer-storage.f" FILE
   s" src/core/engine-error.f" FILE
   s" src/core/exec-vector.f" FILE
   s" src/core/checker-fetch-abi.f" FILE
   s" src/core/checker-owner-extension.f" FILE
   s" src/core/checker-owner-abi.f" FILE
   s" src/habu/created-body-abi.f" FILE
   s" src/habu/prims.f" FILE
   s" src/core/does-clause.f" FILE
   s" src/core/checker.f" FILE
   s" src/core/engine-error-effects.f" FILE
   s" src/core/lower-cert-base.f" FILE
   s" src/habu/native-body-abi.f" FILE
   s" src/habu/engine-origin-early.f" FILE
   s" src/core/type-schema.f" FILE
   s" src/core/type-family.f" FILE
   s" src/core/render.f" FILE
   s" src/core/sumtype.f" FILE
   s" src/core/layout-buffer.f" FILE
   s" src/core/layout-valid.f" FILE
   s" CHECKER-REG:HANDOVER" LINE
   s" src/core/check-hook.f" FILE
   s" src/habu/native-body.f" FILE
   s" src/habu/engine-origin-checked.f" FILE
   s" ENGINE-ORIGIN-EARLY:INSTALL" LINE
   s" src/core/roles.f" FILE
   s" src/core/cell-effects.f" FILE
   s" src/core/declaration-transaction.f" FILE
   s" src/core/generated-declaration.f" FILE
   s" src/core/decl-event.f" FILE
   s" src/core/structure-make.f" FILE
   s" src/core/structure-decl.f" FILE
   s" src/core/arena.f" FILE
   s" src/core/enum-decl.f" FILE
   s" src/core/structures.f" FILE
   s" src/core/bytes.f" FILE
   BOOT-FILES:CORE ['] TARGET-ROW BOOT-FILES:EACH
   s" src/habu/stack-abi.f" FILE
   s" src/habu/layout.f" FILE
   s" src/os/env-base.f" FILE
   s" src/core/quotation-storage.f" FILE
   s" src/core/include.f" FILE
   s" SOURCE-INPUT:RESET" LINE
   ENGINE-ROOT
   S\" s\" src/habu/native-runtime.f\" included" LINE
   s" REQUIRE-BOOT-FREEZE" LINE ;

CAST: >CELL ( n -- ptr n )
CAST: >RESET ( n -- [ -- ] )

\ The retained checker owner resets its own state before the rewind.
: RESET-OWNER ( -- )
   data-base TARGET-CELL + @ {: owner:n :}
   owner 0= if exit then
   owner RESET-OFF + >CELL @ >RESET execute ;

public

: RUN ( -- )
   0 USED !
   STAGE
   RESET-OWNER
   0 TEXT USED @ evaluate-closed
   TEXT-RELEASE ;

;package
