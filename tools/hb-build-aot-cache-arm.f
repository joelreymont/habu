\ ARM exit object for the stored-object cache hit fixture.
require tools/hb-build-test-lib.f

package HB-BUILD-CLI

: HBT-ADD-EXIT-TEXT ( -- )
   ASM-INIT
   0 0 MOVZ,
   NR-EXIT-GROUP SYS,
   CODE ASM-LEN OBJ:TEXT+ ;

;package
