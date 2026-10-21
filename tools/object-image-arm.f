\ ARM64 cached-object image writer. tools/object-image.f loads it for an
\ AArch64 Linux or macOS target, after it has decoded the object.

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

require src/habu/sign-id.f

\ The target sources below are `require`d for the reason the encoders above
\ are: a word probe ("is SYS, / BUILD-IMAGE / DRV-WRITE-IMAGE defined?") asks
\ whether some file defined that word, and an `include` behind a false probe
\ loads a file a second time. tools/native-emit-arm.f `require`s the same
\ target sys.f, elf.f, sign.f, image-bytes.f and driver-io.f, so an image
\ holding both died with `duplicate definition: MAP-ANON-PRIVATE at
\ src/os/linux/sys.f:6`. The require registry is the one record of what this
\ image loaded, and it is keyed by path, so both sites now share it.
: OBJIMG-LOAD-SYS ( -- )
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" required exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" required exit then
   E-OBJ-SCHEMA throw ;

\ Each target writer loads image-bytes.f under its code-package scope.
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

: OBJIMG-LOAD-DRIVER ( -- )
   s" src/habu/driver-io.f" required ;

OBJIMG-LOAD-SYS
OBJIMG-LOAD-IMAGE
OBJIMG-LOAD-DRIVER

package OBJIMG
using A64ICODE

: TEXT>ASM ( -- )
   ASM-INIT
   TEXT-BUF TEXT-U @ BYTES, ;

public

\ The object keeps executable text and immutable DATA payload as separate
\ sections. The latter's address is already bound in the startup's code-base
\ offset and is written after the two GOT cells by the Mach-O writer.
\ This writes the link's image under
\ the identifier that link signs, SIGN-ID:PROG$ (src/habu/sign-id.f says why
\ the identifier is part of the bytes), and a relink is the file a fresh build
\ writes (tools/hb-build-stripped-cache-test.f HBT-STRIPPED-OBJECT-RELINK).
: WRITE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEXT-U @ 0 <= if E-OBJ-SCHEMA throw then
   TEXT>ASM
   DATA-U @ 0 > if
      TEXT-U @ TEXT-BUF TEXT-U @ + DATA-U @ IMG-PAYLOAD!
   then
   SIGN-ID:PROG$ path pathu DRV-EMIT-IMAGE ;

;using
;package
