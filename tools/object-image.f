\ object-image.f - build-internal native image writer for linked OBJ text.
\
\ Load after lib/object-link.f and src/habu/driver-io.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/content-key.f
require lib/object.f
require lib/object-link.f

: OBJIMG-WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

: OBJIMG-LOAD-ASM ( -- )
   s" ASM-INIT" OBJIMG-WORD? if exit then
   s" src/arch/arm64/asm.f" included
   s" src/arch/arm64/icode.f" included
   s" src/arch/arm64/mnem.f" included ;

: OBJIMG-LOAD-SYS ( -- )
   s" SYS," OBJIMG-WORD? if exit then
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" included exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" included exit then
   E-OBJ-SCHEMA throw ;

: OBJIMG-LOAD-TARGET-IMAGE ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/elf.f" included
      s" src/os/linux/sign.f" included
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/macho.f" included
      s" src/os/macos/sign2.f" included
      exit
   then
   E-OBJ-SCHEMA throw ;

: OBJIMG-LOAD-IMAGE ( -- )
   s" BUILD-IMAGE" OBJIMG-WORD? if exit then
   s" src/os/image-bytes.f" included
   OBJIMG-LOAD-TARGET-IMAGE ;

: OBJIMG-LOAD-DRIVER ( -- )
   s" DRV-WRITE-IMAGE" OBJIMG-WORD? if exit then
   s" src/habu/driver-io.f" included ;

OBJIMG-LOAD-ASM
OBJIMG-LOAD-SYS
OBJIMG-LOAD-IMAGE
OBJIMG-LOAD-DRIVER

package OBJIMG

: NONEMPTY-TEXT ( -- )
   OBJLINK:TEXT-SIZE 0 <= if E-OBJ-SCHEMA throw then ;

: TEXT>ASM ( -- )
   ASM-INIT
   OBJLINK:TEXT$ BYTES, ;

public

: RESET ( -- )
   OBJLINK:RESET ;

: ADD ( -- )
   OBJLINK:ADD ;

: WRITE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   OBJLINK:APPLY
   NONEMPTY-TEXT
   TEXT>ASM
   s" hb-obj" path pathu DRV-EMIT-IMAGE ;

end-package
