\ object-image.f - native image writer for one cached, already-linked program.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/content-key.f
require src/lib/object.f

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
\ loads a file a second time. tools/native-emit.f `require`s the same target
\ sys.f, elf.f, sign.f, image-bytes.f and driver-io.f, so an image holding both
\ died with `duplicate definition: MAP-ANON-PRIVATE at src/os/linux/sys.f:6`.
\ The require registry is the one record of what this image loaded, and it is
\ keyed by path, so both sites now share it.
: OBJIMG-LOAD-SYS ( -- )
   HB-TARGET-LINUX? if s" src/os/linux/sys.f" required exit then
   HB-TARGET-MACOS? if s" src/os/macos/sys.f" required exit then
   HB-TARGET-LINUX-X86-64? if s" src/os/linux-x86-64/sys.f" required exit then
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
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/elf.f" required
      s" src/os/linux-x86-64/sign.f" required
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
using OBJ

65 constant UP-A
71 constant UP-G
97 constant LOW-A
103 constant LOW-G

DYNAMIC-BUFFER TEXT-STORAGE n
variable TEXT-U
variable TEXT-ROW
variable EXPORT-ROW
variable DEF-ROW
variable ENTRY-ROW

: TEXT-BUF ( -- ptr u8 )
   0 TEXT-STORAGE byte-view ;

: CLEAR ( -- )
   0 TEXT-U !
   -1 TEXT-ROW !
   -1 EXPORT-ROW !
   -1 DEF-ROW !
   -1 ENTRY-ROW ! ;

: CLAIM ( n ptr n -- ) {: row:n slot:ptr :}
   slot @ 0 >= if E-OBJ-SCHEMA throw then
   row slot ! ;

: SCAN-ROW ( n -- ) {: row:n :}
   row ROW-TAG$ s" source" STR= if exit then
   row ROW-TAG$ s" target" STR= if exit then
   row ROW-TAG$ s" checker" STR= if exit then
   row ROW-TAG$ s" compiler" STR= if exit then
   row ROW-TAG$ s" text" STR= if row TEXT-ROW CLAIM exit then
   row ROW-TAG$ s" export" STR= if row EXPORT-ROW CLAIM exit then
   row ROW-TAG$ s" def" STR= if row DEF-ROW CLAIM exit then
   row ROW-TAG$ s" entry" STR= if row ENTRY-ROW CLAIM exit then
   E-OBJ-SCHEMA throw ;

: SCAN ( -- )
   0 begin dup ROW-COUNT < while
      dup SCAN-ROW
      1+
   repeat drop
   TEXT-ROW @ 0 < if E-OBJ-SCHEMA throw then
   EXPORT-ROW @ 0 < if E-OBJ-SCHEMA throw then
   DEF-ROW @ 0 < if E-OBJ-SCHEMA throw then ;

: CHECK-DEF-OFF ( -- )
   DEF-ROW @ 1 ROW-FIELD$ STR>NUMBER? MATCH option
      none OF E-OBJ-SCHEMA throw ENDOF
      some OF 0 <> if E-OBJ-SCHEMA throw then ENDOF
   ;MATCH ;

: CHECK-ENTRY ( -- )
   EXPORT-ROW @ 0 ROW-FIELD$ {: name:ptr nameu:n :}
   EXPORT-ROW @ 1 ROW-FIELD$ s" --" STR= 0= if E-OBJ-SCHEMA throw then
   DEF-ROW @ 0 ROW-FIELD$ name nameu STR= 0= if E-OBJ-SCHEMA throw then
   DEF-ROW @ 2 ROW-FIELD$ s" --" STR= 0= if E-OBJ-SCHEMA throw then
   CHECK-DEF-OFF
   ENTRY-ROW @ 0 < if
      name nameu s" MAIN" STR= 0= if E-OBJ-SCHEMA throw then
   else
      ENTRY-ROW @ 0 ROW-FIELD$ name nameu STR= 0= if E-OBJ-SCHEMA throw then
   then ;

: HEX-NIB ( n -- n ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and if c STR-ZERO - exit then
   c LOW-A >= c LOW-G < and if c 87 - exit then
   c UP-A >= c UP-G < and if c 55 - exit then
   E-OBJ-FIELD throw ;

: HEX-BYTE ( ptr u8 n -- n ) {: hex:ptr idx:n :}
   hex idx 2 * + c@ HEX-NIB 4 lshift
   hex idx 2 * 1+ + c@ HEX-NIB or ;

: DECODE-BYTE ( ptr u8 n -- ) {: hex:ptr idx:n :}
   hex idx HEX-BYTE TEXT-BUF idx + c! ;

: DECODE ( -- )
   TEXT-ROW @ 0 ROW-FIELD$ {: hex:ptr hexu:n :}
   hexu 2 / {: bytes:n :}
   bytes 0 <= if E-OBJ-SCHEMA throw then
   bytes CELL / bytes CELL mod 0 > if 1+ then
   TEXT-STORAGE-RESERVE
   0 begin dup bytes < while
      dup hex swap DECODE-BYTE
      1+
   repeat drop
   bytes TEXT-U ! ;

: TEXT>ASM ( -- )
   ASM-INIT
   TEXT-BUF TEXT-U @ BYTES, ;

public

\ The object codec validates external record syntax; the cache also checks its
\ content key. This boundary accepts only the program shape hb-build emits.
: LOAD ( -- )
   CLEAR
   SCAN
   CHECK-ENTRY
   DECODE ;

\ An object's text is a stripped program as the maker's link emitted it
\ (src/habu/aot-lib.f LINK writes both), so this writes that link's image under
\ the identifier that link signs, SIGN-ID:PROG$ (src/habu/sign-id.f says why
\ the identifier is part of the bytes), and a relink is the file a fresh build
\ writes (tools/hb-build-stripped-cache-test.f HBT-STRIPPED-OBJECT-RELINK).
: WRITE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEXT-U @ 0 <= if E-OBJ-SCHEMA throw then
   TEXT>ASM
   SIGN-ID:PROG$ path pathu DRV-EMIT-IMAGE ;

;using
;using
;package
