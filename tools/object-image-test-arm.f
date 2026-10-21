\ ARM exit text for the object-image fixture: tools/object-image-test.f loads
\ it for an AArch64 target, after tools/object-image.f has loaded the ARM
\ writer and its encoders.
require src/lib/object.f

package OBJIMG-TEST
using A64ICODE

: EXIT-CODE ( -- )
   ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS, ;

\ Bytes after the exit stub, which never runs them.
: CODE-PAD ( ptr u8 n -- )
   BYTES, ;

: CODE-TEXT+ ( -- )
   CODE ASM-LEN OBJ:TEXT+ ;

;using
;package
