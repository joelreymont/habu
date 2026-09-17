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
' LOAD-SYS
;package
execute

require src/os/script-argv.f
require src/habu/treeshake.f
require src/habu/rt.f
require src/habu/crash.f
require src/os/image-bytes.f

package NATIVE-EMIT
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
require src/habu/driver-io.f
require tools/native-layout.f

package NATIVE-EMIT

private

: TRANSLATE-FIXED ( ptr n n -- ) {: host:ptr count:n :}
   NATIVE-LAYOUT:CURRENT DATA-START NATIVE-LAYOUT:CHECK
   AOT-WINDOW:XTOFF-N @ 0 ?do
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * + CELL-VIEW {: row:ptr :}
      row @ {: pair:n :}
      \ Each row is a 32-bit location followed by 32-bit typed target metadata.
      pair $FFFFFFFF and {: loc:n :}
      loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0= if
         host count loc pair 32 rshift AOT-WINDOW:XTOFF-DATA-TAG and 0<>
         NATIVE-LAYOUT:TRANSLATE
         pair $FFFFFFFF00000000 and or row !
      then
   loop ;

public

: WRITE ( AOT-OWNED:capture ptr n n ptr u8 n -- ) {: host:ptr count:n path:ptr size:n :}
   dup AOT-OWNED:ORIGIN@ {: origin:n :}
   AOT-FILE:IMPORT
   host count TRANSLATE-FIXED
   0 0= STDIN? !
   NULL$ origin ENGINE-EMIT:FORTH-ORIGIN
   s" hb" path size DRV-EMIT-IMAGE ;

;package
