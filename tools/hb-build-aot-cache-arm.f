\ ARM exit object for the stored-object cache hit fixture.
require tools/hb-build-test-lib.f
require src/arch/arm64/icode.f

package HB-BUILD-CLI

: HBT-ADD-EXIT-TEXT ( -- )
   A64ICODE:ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS,
   A64ICODE:CODE A64ICODE:ASM-LEN OBJ:TEXT+ ;

;package
