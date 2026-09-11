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

\ The running host may predate a new engine callback. Keep its actual engine
\ declarations, then discard every declaration belonging to the retired heap.
: RESET-ADDRESS-ROWS ( -- )
   0
   begin
      dup data-base SNAP-RELOC:XTCELL-N-CELL + @ <
   while
      dup cells data-base SNAP-RELOC:XTCELL-ROWS-OFF + + @
      SNAP-RELOC:XTCELL-OFF-MASK and DATA-START >= if
         data-base SNAP-RELOC:XTCELL-N-CELL + ! exit
      then
      1+
   repeat
   data-base SNAP-RELOC:XTCELL-N-CELL + ! ;

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
