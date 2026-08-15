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

\ The three ARM64 encoder sources are `require`d, not probed-and-included.
\ A probe on ASM-INIT decided whether to load asm.f, but ASM-INIT is defined in
\ icode.f, so the probe only ever answered "is icode.f loaded" - it reported
\ asm.f absent whenever the two were not loaded together, and loading asm.f a
\ second time is a duplicate definition, not a no-op. `require` asks the
\ registry that actually records what is loaded, which is also the registry the
\ ten-plus existing `require src/arch/arm64/asm.f` sites already share.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

\ driver-io.f's DRV-WRITE-IMAGE-PATH calls FDIO:WALL, and for the same reason the
\ encoders above are `require`d rather than probed: the DRV-WRITE-IMAGE probe
\ below answers "is driver-io.f loaded", which is a different question.
require src/habu/fdio.f

: OBJIMG-WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

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

\ driver-io.f's DRV-SIZE-MAP references the ENGINE-SIZE table, so it must load
\ first. An object image never runs ENGINE-EMIT:FORTH, so the table stays empty and
\ DRV-SIZE-MAP skips - this only satisfies the compile-time dependency.
: OBJIMG-LOAD-SIZE ( -- )
   s" ENGINE-SIZE:MARK" OBJIMG-WORD? if exit then
   s" src/habu/engine-size.f" included ;

: OBJIMG-LOAD-DRIVER ( -- )
   s" DRV-WRITE-IMAGE" OBJIMG-WORD? if exit then
   s" src/habu/driver-io.f" included ;

OBJIMG-LOAD-SYS
OBJIMG-LOAD-IMAGE
OBJIMG-LOAD-SIZE
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

;package
