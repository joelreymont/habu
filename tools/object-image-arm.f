\ ARM64 cached-object image writer. The object's instruction stream is wrapped
\ and signed in the target container before publication.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/fdio.f
require src/habu/sign-id.f

: OBJIMG-LOAD-SYS ( -- )
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" required exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" required exit then
   E-OBJ-SCHEMA throw ;

: OBJIMG-LOAD-IMAGE ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/elf.f" required
      s" src/os/linux/sign.f" required
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/macho.f" required
      s" src/os/macos/sign2.f" required
      exit
   then
   E-OBJ-SCHEMA throw ;

OBJIMG-LOAD-SYS
OBJIMG-LOAD-IMAGE
require src/habu/driver-io.f

package OBJIMG

: TEXT>ASM ( -- )
   ASM-INIT
   OBJLINK:TEXT$ BYTES, ;

public

: WRITE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   OBJLINK:APPLY
   NONEMPTY-TEXT
   TEXT>ASM
   SIGN-ID:PROG$ path pathu DRV-EMIT-IMAGE ;

;package
