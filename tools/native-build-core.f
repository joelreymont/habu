\ Shared native-build driver. Entries select and own the compilation tier.

require lib/errors.f
require src/core/prefix-boundary.f
require lib/string.f
require lib/memory.f
require src/habu/address-cells.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require src/os/script-argv.f
require lib/codesign.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f
require tools/native-layout.f

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

: ADDR-ROW@ ( n -- n ) ADDRESS-CELLS:ROW@ ;

: ADDR-ROW-OFF ( n -- n ) ADDR-ROW@ SNAP-RELOC:XTCELL-OFF-MASK and ;

: HOST-HEAP-START ( -- n )
   data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

: CHECK-HOST-LAYOUT ( -- )
   NATIVE-LAYOUT:CURRENT HOST-HEAP-START NATIVE-LAYOUT:CHECK
   ADDR-ROWS 0 ?do
      i ADDR-ROW@ {: raw:n :}
      raw SNAP-RELOC:XTCELL-OFF-MASK and {: off:n :}
      off HOST-HEAP-START < if
         NATIVE-LAYOUT:CURRENT off raw SNAP-RELOC:XTCELL-DATA-TAG and 0<>
         NATIVE-LAYOUT:TRANSLATE drop
      then
   loop ;

: KEEP-ROWS-BELOW ( n -- ) ADDRESS-CELLS:KEEP-BELOW ;

: RESET-ADDRESS-ROWS ( -- )
   HOST-HEAP-START {: floor:n :}
   floor 0= floor DATA-START <> or if
      s" native-build: host layout does not match its published heap floor" 76 die
   then
   floor KEEP-ROWS-BELOW ;

TRUSTED: RESET-XT ( n -- [ -- ] ) ;
TRUSTED: IMPORT-XT ( n -- [ ptr u8 -- ] ) ;

\ Resolve the current source owner in its own package. The cold seed uses the
\ native dispatch header cells for its old compiler's temporary stack.
TRUSTED: CHECKER-OWNER ( -- ptr u8 )
   s" package CHECKER-REG DECLARATIONS ;package" evaluate ;

\ These execution tokens belong to the retained/target private checker owners.
TRUSTED: RESET-CHECKER ( ptr u8 -- ) {: owner:ptr :}
   owner 0= if exit then
   owner NCOMP-DISPATCH:DECL-RESET-OFF + CELL-VIEW @ RESET-XT execute ;

TRUSTED: TRANSFER-CHECKER ( ptr u8 -- ) {: source:ptr :}
   CHECKER-OWNER {: owner:ptr :}
   owner 0= if s" native-build: target checker owner missing" 76 die then
   source owner NCOMP-DISPATCH:DECL-TRANSFER-OFF + CELL-VIEW @ IMPORT-XT execute ;

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
   s" src/core/checker-owner-abi.f" included
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
   \ Transfer publishes the complete replacement owner. Earlier pre-hook code
   \ still needs the retained compiler's paired checker; later calls use this one.
   CHECKER-OWNER:CAPTURE-PREPARE
   CHECKER-OWNER AOT-ARM:PAYLOAD-PERSISTENT
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
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   NSTR:WINDOW-OPEN
   LOAD-TARGET
   AOT-ARM:WINDOW-CLOSE ;

\ Resolve a target operation without compiling another definition or opening a
\ source frame. It must be part of the frozen target code window.
: TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-build: target operation missing" 76 die
   then
   XREF-START {: xt:n :}
   xt AOT-ARM:B0 @ < xt AOT-ARM:B1 @ >= or if
      s" native-build: operation does not belong to target" 76 die
   then
   xt ;

TRUSTED: PREPARE-XT ( n -- [ -- ] ) ;

: PREPARE-TARGET ( -- )
   s" NATIVE-RUNTIME:CAPTURE-PREPARE" TARGET-XT PREPARE-XT execute
   here AOT-ARM:D1 ! ;

\ A retained driver cell is not a fixed engine slot. Registering a target XT
\ into one after reset would make it look like a load-time engine hook.
: CHECK-FIXED-ROWS ( -- )
   AOT-WINDOW:XTOFF-N @ 0 ?do
      \ The first four bytes of each eight-byte row encode its location.
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * + CELL-VIEW @ $FFFFFFFF and
      dup AOT-WINDOW:XTOFF-WINDOW-TAG and 0= if
         HOST-HEAP-START CELL - > if
            S\" native-build: captured fixed address belongs to the retired host heap\n" BUILD-RC die
         then
      else drop then
   loop ;

: CAPTURE ( -- )
   AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   CHECK-FIXED-ROWS ;

FS-PATH-CAP constant OUTPUT-CAP
create OUTPUT-BUF OUTPUT-CAP allot
variable OUTPUT-U
create TEMP-BUF OUTPUT-CAP allot
variable TEMP-U
create SMOKE-DIR OUTPUT-CAP allot
variable SMOKE-DIR-U

: OUTPUT$ ( -- ptr u8 n ) OUTPUT-BUF OUTPUT-U @ ;
: TEMP$ ( -- ptr u8 n ) TEMP-BUF TEMP-U @ ;
: TEMP-SUFFIX$ ( -- ptr u8 n ) s" .native-build.tmp" ;

: ABSOLUTE-OUTPUT! ( ptr u8 n -- ) {: path:ptr size:n :}
   size 0 <= size OUTPUT-CAP > or if E-FS-PATH throw then
   path c@ 47 = if
      path OUTPUT-BUF size BYTE-COPY size OUTPUT-U ! exit
   then
   SOURCE-ROOT:CWD$ path size OUTPUT-BUF JOIN-PATH OUTPUT-U ! ;

: OUTPUT! ( ptr u8 n -- ) {: path:ptr size:n :}
   path size ABSOLUTE-OUTPUT!
   OUTPUT-U @ {: absoluteu:n :}
   absoluteu OUTPUT-CAP TEMP-SUFFIX$ nip - > if E-FS-PATH throw then
   OUTPUT-BUF TEMP-BUF absoluteu BYTE-COPY
   TEMP-SUFFIX$ TEMP-BUF absoluteu + swap BYTE-COPY
   absoluteu TEMP-SUFFIX$ nip + TEMP-U ! ;

: SMOKE-DIR! ( -- )
   s" hb-native-smoke" TMPDIR-MKDIR {: path:ptr size:n :}
   path size CLEANUP-TREE+
   path SMOKE-DIR size BYTE-COPY size SMOKE-DIR-U ! ;

\ This private boundary has the source writer's exact compiled ABI. Its capture
\ is a multi-cell value, which cannot be passed through interpret-mode evaluate.
TRUSTED: WRITER-XT ( n -- [ AOT-OWNED:capture ptr n n ptr u8 n -- ] ) ;

: SOURCE-WRITER ( -- [ AOT-OWNED:capture ptr n n ptr u8 n -- ] )
   s" NATIVE-EMIT:WRITE" XREF-FIND dup XREF-FOUND? 0= if
      drop S\" native-build: source writer missing\n" BUILD-RC die
   then
   dup XREF-RETIRED? if
      drop S\" native-build: source writer retired\n" BUILD-RC die
   then
   XREF-START {: xt:n :}
   \ Resolve after loading: neither the retained host nor captured target owns
   \ this writer. It was compiled above the frozen target window.
   xt 0= xt AOT-ARM:B1 @ < or xt cp@ >= or if
      S\" native-build: writer does not belong to the source load\n" BUILD-RC die
   then
   xt WRITER-XT ;

\ The reader's capture has its own bytes. Loading a writer may allocate and
\ compile freely; none of those definitions or mutations enters that value.
: WRITE-TARGET ( AOT-OWNED:capture -- )
   s" tools/native-emit.f" required
   NATIVE-LAYOUT:CURRENT TEMP$ SOURCE-WRITER execute ;

: WRITE-OWNED ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup WRITE-TARGET ;

: EMIT-TEMP ( [ n n -- n ] bool -- )
   AOT-ARM:B0 @ AOT-ARM:B1 @ 2swap AOT-FILE:OWN-WINDOW ['] WRITE-OWNED catch {: rc:n :}
   AOT-OWNED:CLOSE
   rc 0<> if rc throw then
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
   TEMP$ >LEN
   SMOKE-DIR SMOKE-DIR-U @ >LEN
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
   TEMP$ OUTPUT$ RENAME-FILE ;

: REMOVE-STALE-TEMP ( -- )
   TEMP$ 2dup SYMLINK? if REMOVE-FILE exit then
   2dup EXISTS? if REMOVE-FILE else 2drop then ;

: DRIVE ( [ n n -- n ] bool -- [ n n -- n ] bool ) {: query bootstrap:bool :}
   CHECK-HOST-LAYOUT
   CHECKER-OWNER {: source:ptr :}
   source LOGICAL-RESET
   source OPEN-AND-COMPILE
   AOT-CAPTURE:PAYLOAD-CAPTURE
   PREPARE-TARGET
   CAPTURE
   query bootstrap EMIT-TEMP
   SIGN-TEMP
   SMOKE
   PROMOTE
   query bootstrap ;

public

: RUN ( [ n n -- n ] bool -- ) {: query bootstrap:bool :}
   CLEANUP-RESET
   SCRIPT-ARGC 1 <> if
      S\" native-build: one explicit output path is required\n" BUILD-RC die
   then
   0 SCRIPT-ARGV$ OUTPUT!
   REMOVE-STALE-TEMP
   TEMP$ CLEANUP+
   SMOKE-DIR!
   query bootstrap [: DRIVE ;] catch {: rc:n :}
   2drop
   CLEANUP-RUN
   rc 0<> if s" native-build: uncaught throw code " type rc . cr then
   s" " rc die ;

;package
