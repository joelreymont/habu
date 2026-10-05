\ ARM exit text for the object-image fixture.
require lib/object.f

package OBJIMG-TEST

: EXIT-TEXT ( -- )
   ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS,
   CODE ASM-LEN OBJ:TEXT+ ;

;package
