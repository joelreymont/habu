\ icode-capacity.f - arm64 label/fixup table capacity regression.
\ Run: bin/hb --load test/icode-capacity.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f

package ICODE-CAPACITY

$1000 constant RETIRED-CAP
$2000 constant ACTIVE-CAP
$1000 constant LABEL-CAP
$48 constant ICODE-RC
$7 constant TEST-LABEL
$2 constant TEST-KIND
$100 constant CAPTURE-CAP
60000 constant TIMEOUT-MS
create OUT CAPTURE-CAP allot
create ERR CAPTURE-CAP allot

: APPEND-TO ( n -- ) {: cap:n :}
   begin NFX @ cap < while
      NFX @ TEST-LABEL >LABEL TEST-KIND FX+
   repeat ;

: ASSERT-ROW ( n -- ) {: idx:n :}
   idx cells FXS + @ idx T=
   idx cells FXL + @ TEST-LABEL T=
   idx cells FXK + @ TEST-KIND T= ;

: OVERFLOW ( -- )
   NFX @ TEST-LABEL >LABEL TEST-KIND FX+ ;

: ASSERT-LABEL-CAP ( -- )
   ASM-INIT
   LBL LABEL>N 0 T=
   LABEL-CAP 2 - 0 ?do LBL LABEL>N drop loop
   LBL LABEL>N LABEL-CAP 1- T=
   NLBL @ LABEL-CAP T= ;

: MODE? ( -- bool )
   s" HABU_ICODE_OVERFLOW" GETENV nip 0 > ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb"
   then ;

: ASSERT-OVERFLOW ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" HABU_ICODE_OVERFLOW" >LEN s" 1" >LEN PROC-ENV+
   s" --load" >LEN PROC-ARGV+
   s" test/icode-capacity.f" >LEN PROC-ARGV+
   HB$ >LEN
   OUT CAPTURE-CAP >LEN ERR CAPTURE-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu:len erru:len code:rc :}
   code RC>N ICODE-RC T=
   outu LEN>N 0 T=
   ERR erru LEN>N s" icode: out of fixups" T$= ;

public

: RUN ( -- )
   T-RESET
   ASSERT-LABEL-CAP
   ASM-INIT
   RETIRED-CAP APPEND-TO
   NFX @ RETIRED-CAP T=
   RETIRED-CAP 1- ASSERT-ROW
   ACTIVE-CAP APPEND-TO
   NFX @ ACTIVE-CAP T=
   ACTIVE-CAP 1- ASSERT-ROW
   ASSERT-OVERFLOW
   T-REPORT ;

: ENTRY ( -- )
   MODE? if
      ASM-INIT
      ACTIVE-CAP APPEND-TO
      OVERFLOW
      exit
   then
   RUN ;

;package

ICODE-CAPACITY:ENTRY
