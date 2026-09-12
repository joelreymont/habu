\ native-build.f - rebuild the complete native runtime in one AOT window.

require lib/errors.f
require src/core/prefix-boundary.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/codesign.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

package NATIVE-BUILD

: LOAD-SYS ( -- )
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" required exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" required exit then
   s" native-build: unknown target" 76 die ;

: LOAD-IMAGE ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/elf.f" required
      s" src/os/linux/sign.f" required
      s" src/os/linux/proc-watch.f" required
      s" src/os/linux/proc-control.f" required
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/macho.f" required
      s" src/os/macos/sign2.f" required
      s" src/os/macos/proc-watch.f" required
      s" src/os/macos/proc-control.f" required
      exit
   then
   s" native-build: unknown target" 76 die ;

public

: LOAD-HOST-TARGET ( -- )
   LOAD-SYS ;

: LOAD-HOST-IMAGE ( -- )
   LOAD-IMAGE ;

;package

NATIVE-BUILD:LOAD-HOST-TARGET

require src/os/script-argv.f
require src/habu/treeshake.f
require src/habu/rt.f
require src/habu/crash.f
require src/os/image-bytes.f

NATIVE-BUILD:LOAD-HOST-IMAGE

require src/habu/regalloc.f
require src/habu/habu1.f
require src/habu/jit.f
require src/habu/prof.f
require src/habu/fdio.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/habu2.f
require src/habu/driver-io.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f

package NATIVE-BUILD

$4A constant BUILD-RC
$1000 constant SMOKE-CAP
10000 constant SMOKE-TIMEOUT-MS

create SMOKE-OUT SMOKE-CAP allot
create SMOKE-ERR SMOKE-CAP allot

\ The running host may predate a new engine callback, and its reserved bands may be
\ smaller than this tree's. Keep the host's actual engine declarations -- the cells
\ below ITS heap floor -- and discard every declaration belonging to the retired
\ heap above it.
\
\ THE BOUNDARY IS THE HOST'S, NOT THE TREE'S. A tree whose reserved bands grew
\ moves DATA-START and the host that builds it did not move with it, so classifying
\ the host's rows by the source constant keeps the host's own heap rows, which the
\ capture then refuses by value (src/habu/aot-capture.f ACAP-TARGET-REFUSE, exit
\ 74). The engine publishes its floor at boot in BOOT-LAYOUT:HEAP-START-CELL.
\
\ FILTERED, NOT TRUNCATED. A fixed-band cell is registered where its kind is
\ decided, and for two of them that happens after the heap has started growing, so
\ the rows are NOT partitioned in registration order -- measured 2026-09-12, a
\ product engine carries 9 engine rows of which 2 sit past its first heap row, and
\ stopping at the first heap row dropped them. Compacting in place keeps every
\ engine row and preserves the order the registrar wrote.
: ADDR-ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: ADDR-ROWS! ( n -- ) data-base SNAP-RELOC:XTCELL-N-CELL + ! ;

: ADDR-ROW@ ( n -- n ) {: k:n :}
   k cells data-base SNAP-RELOC:XTCELL-ROWS-OFF + + @ ;

: ADDR-ROW! ( n n -- ) {: k:n row:n :}
   row k cells data-base SNAP-RELOC:XTCELL-ROWS-OFF + + ! ;

: ADDR-ROW-OFF ( n -- n ) ADDR-ROW@ SNAP-RELOC:XTCELL-OFF-MASK and ;

\ The host's own heap floor, or 0 from a host built before the cell existed. Read
\ through habu2.f's host-side mirror of the offset, for the reason stated there:
\ this file compiles against the HOST's dictionary, which on a pre-cell host has
\ no BOOT-LAYOUT to name.
: HOST-HEAP-START ( -- n ) data-base EM-LAYOUT:HEAP-START-OFF + @ ;

: KEEP-ROWS-BELOW ( n -- ) {: floor:n :}
   0 ADDR-ROWS 0 ?do
      i ADDR-ROW@ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and floor < if
         dup row ADDR-ROW! 1+
      then
   loop
   ADDR-ROWS! ;

\ THE FALLBACK'S BOUND IS STRUCTURAL, NOT A THRESHOLD. $7FF8 is the highest DATA
\ offset emitted code can name with `DATA <off> LDR` -- a 12-bit immediate scaled
\ by eight -- the ceiling src/habu/layout.f states at every band that had to stay
\ under it. Every address cell the ENGINE declares is a cell its own compiled code
\ names that way: the hook cells, the native dispatch cells and the application
\ entry. So an engine declaration is at or below this bound while a DP-heap row is
\ far above it (measured 2026-09-12: 17312 for the highest engine cell against a
\ heap floor of 958280). A row the fallback KEPT above the bound therefore says the
\ host's reserved bands are smaller than this tree's and the source constant has
\ misclassified the host's heap, so the build refuses by name instead of baking a
\ retired host address. A host LARGER than the tree is the one direction this
\ cannot see; docs/bootstrap.md carries that as the landing rule.
$7FF8 constant FIXED-CELL-MAX

: FALLBACK-REFUSE ( n -- ) {: off:n :}
   s" native-build: host predates BOOT-LAYOUT:HEAP-START-CELL and its kept row DATA+" type off .
   s"  is not an engine cell" type cr
   s" native-build: host heap start unknown; build from a post-cell host (docs/bootstrap.md)" 76 die ;

: CHECK-FALLBACK-ROWS ( -- )
   ADDR-ROWS 0 ?do
      i ADDR-ROW-OFF {: off:n :}
      off FIXED-CELL-MAX > if off FALLBACK-REFUSE then
   loop ;

: RESET-ADDRESS-ROWS ( -- )
   HOST-HEAP-START {: floor:n :}
   floor 0<> if floor KEEP-ROWS-BELOW exit then
   DATA-START KEEP-ROWS-BELOW
   CHECK-FALLBACK-ROWS ;

defer RESET-SOURCE ( -- )
defer IMPORT-CHECKED ( ptr u8 -- )

\ Resolve the current source owner in its own package. The cold seed uses the
\ native dispatch header cells for its old compiler's temporary stack.
TRUSTED: CHECKER-OWNER ( -- ptr u8 )
   s" package CHECKER-REG DECLARATIONS ;package" evaluate ;

\ These execution tokens belong to the retained/target private checker owners.
TRUSTED: RESET-CHECKER ( ptr u8 -- ) {: owner:ptr :}
   owner 0= if exit then
   owner NCOMP-DISPATCH:DECL-RESET-OFF + CELL-VIEW @ is RESET-SOURCE
   RESET-SOURCE ;

TRUSTED: TRANSFER-CHECKER ( ptr u8 -- ) {: source:ptr :}
   CHECKER-OWNER {: owner:ptr :}
   owner 0= if s" native-build: target checker owner missing" 76 die then
   owner NCOMP-DISPATCH:DECL-TRANSFER-OFF + CELL-VIEW @ is IMPORT-CHECKED
   source IMPORT-CHECKED ;

\ The discarded build host remains callable through this compiled continuation,
\ but none of its dictionary records or address declarations enters the window.
TRUSTED: LOGICAL-RESET ( ptr u8 -- )
   0 set-check
   0 set-top-check
   RESET-CHECKER
   CORE-PREFIX:FIRST-RECORD seed-ndict!
   RESET-ADDRESS-ROWS ;

: LOAD-TARGET ( ptr u8 -- ) {: source:ptr :}
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/pointer-storage.f" included
   s" src/core/engine-error.f" included
   s" src/core/exec-vector.f" included
   s" src/core/checker.f" included
   s" src/core/engine-error-effects.f" included
   s" src/core/lower-cert-base.f" included
   s" src/core/type-schema.f" included
   s" src/core/type-family.f" included
   s" src/core/render.f" included
   s" src/core/sumtype.f" included
   s" src/core/layout-buffer.f" included
   s" src/core/layout-valid.f" included
   source TRANSFER-CHECKER
   s" src/core/check-hook.f" included
   s" src/core/roles.f" included
   s" src/core/cell-effects.f" included
   s" src/core/declaration-transaction.f" included
   s" src/core/generated-declaration.f" included
   s" src/core/decl-event.f" included
   s" src/core/structure-make.f" included
   s" src/core/structure-decl.f" included
   s" src/core/enum-decl.f" included
   s" src/core/structures.f" included
   s" src/core/bytes.f" included
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" included
      s" src/os/linux/layout.f" included
   else HB-TARGET-MACOS? if
      s" src/os/macos/target.f" included
      s" src/os/macos/layout.f" included
   else
      s" native-build: unknown target" 76 die
   then then
   s" src/habu/layout.f" included
   s" src/os/env-base.f" included
   s" src/core/include.f" included
   s" src/habu/native-runtime.f" included ;

: OPEN-AND-COMPILE ( ptr u8 -- )
   AOT-ARM:WINDOW-OPEN
   NSTR:WINDOW-OPEN
   LOAD-TARGET
   AOT-ARM:WINDOW-CLOSE ;

: CAPTURE ( -- )
   AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE ;

: TEMP$ ( -- ptr u8 n )
   s" bin/.hb-native-build.tmp" ;

: EMIT-TEMP ( -- )
   0 0= STDIN? !
   NULL$ ENGINE-EMIT:FORTH
   s" hb" TEMP$ DRV-EMIT-IMAGE
   TEMP$ CHMOD-X ;

: SMOKE-RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF
         PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0
      ENDOF
      err OF
         PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N
      ENDOF
   ;MATCH ;

: SMOKE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" ./.hb-native-build.tmp" >LEN
   s" bin" >LEN
   S\" : X ( -- n ) 42 ; X . cr\n" >LEN
   SMOKE-OUT SMOKE-CAP >LEN SMOKE-ERR SMOKE-CAP >LEN SMOKE-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE SMOKE-RESULT {: outu:n erru:n rc:n :}
   rc 0<> if
      SMOKE-ERR erru type
      BUILD-RC throw
   then
   erru 0<> if
      SMOKE-ERR erru type
      BUILD-RC throw
   then
   SMOKE-OUT outu S\" 42\n\n" STR= 0= if
      BUILD-RC throw
   then ;

: SIGN-TEMP ( -- )
   TEMP$ CODESIGN:ENSURE ;

: PROMOTE ( -- )
   TEMP$ s" bin/hb" RENAME-FILE ;

: ENSURE-BIN ( -- )
   s" bin" DIR? 0= if s" bin" MAKE-DIR then ;

: REMOVE-STALE-TEMP ( -- )
   TEMP$ 2dup SYMLINK? if REMOVE-FILE exit then
   2dup EXISTS? if REMOVE-FILE else 2drop then ;

: DRIVE ( -- )
   CHECKER-OWNER {: source:ptr :}
   source LOGICAL-RESET
   source OPEN-AND-COMPILE
   CAPTURE
   EMIT-TEMP
   SIGN-TEMP
   SMOKE
   PROMOTE ;

public

: RUN ( -- )
   CLEANUP-RESET
   ENSURE-BIN
   REMOVE-STALE-TEMP
   TEMP$ CLEANUP+
   [: DRIVE ;] catch {: rc:n :}
   CLEANUP-RUN
   rc 0= if DRV-EXIT-OK then
   rc DRV-FAIL ;

;package

NATIVE-BUILD:RUN
