\ Source-bound emission, loaded only after the target capture is complete.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

package NATIVE-EMIT
: LOAD-SYS ( -- )
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" required exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" required exit then
   \ The linux-x86-64 seam's emitters are written against package X64ASM the way
   \ the other two are written against A64ASM's mnemonics, and only a build for
   \ that target pays for it.
   HB-TARGET-LINUX-X86-64? if
      s" src/arch/x86-64/asm.f" required
      s" src/os/linux-x86-64/sys.f" required exit
   then
   s" native-emit: unknown target" 76 die ;
' LOAD-SYS
;package
execute

require src/os/script-argv.f
require src/habu/treeshake.f
require src/habu/rt.f
require src/habu/crash.f
\ The target writers load their image buffer in their own package scope.
package NATIVE-EMIT
: LOAD-IMAGE ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/elf.f" required
      s" src/os/linux/sign.f" required
      s" src/os/linux/proc-watch.f" required
      s" src/os/linux/proc-control.f" required exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/macho.f" required
      s" src/os/macos/sign2.f" required
      s" src/os/macos/proc-watch.f" required
      s" src/os/macos/proc-control.f" required exit
   then
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/elf.f" required
      s" src/os/linux-x86-64/sign.f" required
      s" src/os/linux-x86-64/proc-watch.f" required
      s" src/os/linux-x86-64/proc-control.f" required exit
   then
   s" native-emit: unknown target" 76 die ;
' LOAD-IMAGE
;package
execute

require src/habu/regalloc.f
require src/habu/habu1.f
require src/habu/jit.f
require src/habu/prof.f
require src/habu/fdio.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/aot-owned.f
require src/habu/habu2.f
require src/habu/sign-id.f
require src/habu/driver-io.f
require tools/native-layout.f

using AOT-BUF
package NATIVE-EMIT

private

: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;

: REC ( n -- ptr u8 )
   AOT-CREC-ROW * AOT-REC-MAX 48 * + AOT-REC-BUF@ + ;

: NAME= ( ptr u8 ptr u8 n -- bool ) {: rec:ptr a:ptr u:n :}
   rec 8 + W32@ {: off:n :}
   off AOT-NAMES-LEN @ >= if s" native-emit: invalid scope name" 74 die then
   AOT-NAMES-BUF@ off + {: name:ptr :}
   name c@ {: len:n :}
   len AOT-NAMES-LEN @ off - 1- > if
      s" native-emit: invalid scope name" 74 die
   then
   name 1+ len a u STR= ;

: PACKAGE-PUBLIC-WID ( ptr u8 n -- n ) {: name:ptr size:n :}
   AOT-REC-N @ 0 ?do
      i REC {: rec:ptr :}
      rec 16 + W32@ $FFFFFFFF = if
         rec name size NAME= if rec W32@ unloop exit then
      then
   loop
   s" native-emit: C2 namespace missing" 74 die ;

: PACKAGE-CODE-OFF ( n ptr u8 n -- n ) {: wid:n name:ptr size:n :}
   AOT-REC-N @ 0 ?do
      i REC {: rec:ptr :}
      rec 16 + W32@ wid = if
         rec name size NAME= if
            rec W32@ {: off:n :}
            rec 4 + W32@ CODE-SPAN:BYTES {: len:n :}
            len 0 <= off AOT-BLOB-LEN @ >= or
            off len + AOT-BLOB-LEN @ > or if
               s" native-emit: C2 scope code outside capture" 74 die
            then
            off unloop exit
         then
      then
   loop
   s" native-emit: C2 scope member missing" 74 die ;

: C2-WID ( -- n ) s" C2-MEM" PACKAGE-PUBLIC-WID ;
: REAL-READ-CODE-OFF ( -- n ) C2-WID s" WITH-READ" PACKAGE-CODE-OFF ;
: REAL-MUT-CODE-OFF ( -- n ) C2-WID s" WITH-MUT" PACKAGE-CODE-OFF ;
: REAL-MUT-LOAN-CODE-OFF ( -- n ) C2-WID s" WITH-MUT-LOAN" PACKAGE-CODE-OFF ;
: REAL-INIT-CODE-OFF ( -- n ) C2-WID s" WITH-INIT" PACKAGE-CODE-OFF ;
: REAL-RECORDS-CODE-OFF ( -- n ) C2-WID s" WITH-RECORDS" PACKAGE-CODE-OFF ;
: REAL-RECORD-CODE-OFF ( -- n ) C2-WID s" WITH-RECORD" PACKAGE-CODE-OFF ;
: REAL-FIELD-CODE-OFF ( -- n ) C2-WID s" WITH-FIELD" PACKAGE-CODE-OFF ;

public

: WRITE ( AOT-OWNED:capture ptr n n ptr u8 n -- ) {: host:ptr count:n path:ptr size:n :}
   dup AOT-OWNED:ORIGIN@ {: origin:n :}
   AOT-FILE:IMPORT
   host count NATIVE-LAYOUT:TRANSLATE-ROWS
   0 0= STDIN? !
   NULL$ origin ENGINE-EMIT:FORTH-ORIGIN
   SIGN-ID:ENGINE$ path size DRV-EMIT-IMAGE ;

: WRITE-C2 ( AOT-OWNED:capture ptr n n ptr u8 n -- ) {: host:ptr count:n path:ptr size:n :}
   dup AOT-OWNED:ORIGIN@ {: origin:n :}
   AOT-FILE:IMPORT
   AOT-RUNTIME:COMPLETE? 0= if
      s" native-emit: C2 entries require complete runtime" 74 die
   then
   REAL-READ-CODE-OFF {: read:n :}
   REAL-MUT-CODE-OFF {: mut:n :}
   REAL-MUT-LOAN-CODE-OFF {: loan:n :}
   REAL-INIT-CODE-OFF {: init:n :}
   REAL-RECORDS-CODE-OFF {: records:n :}
   REAL-RECORD-CODE-OFF {: record:n :}
   REAL-FIELD-CODE-OFF {: field:n :}
   host count NATIVE-LAYOUT:TRANSLATE-ROWS
   0 0= STDIN? !
   NULL$ origin read mut loan init records record field ENGINE-EMIT:FORTH-C2-ORIGIN
   s" hb" path size DRV-EMIT-IMAGE ;

;package
;using
