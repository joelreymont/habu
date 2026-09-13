\ Real capture entry and real package allocation; no synthetic owner callbacks.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

\ Read-only access to the existing one-entry memo, for its absent-to-live case.
package AOT-CAPTURE
public
: PAYLOAD-TEST-PRIME ( n -- )
   ACAP-PKG-LOOKUP drop -1 <> if 79 throw then ;
;package

package PAYLOAD-ADMISSION-TEST

\ The fixed programs below contain declarations only and leave no stack values.
TRUSTED: DEFINITIONS ( ptr u8 n -- ) evaluate ;

: TRUE! ( bool -- ) 0= if 79 throw then ;

: END-BOUNDS ( -- )
   cp@ AOT-ARM:B1 ! ndict@ AOT-ARM:R1 !
   here AOT-ARM:D1 ! AOT-ARM:WIDN AOT-ARM:W1 ! ;

: PRIME-NEXT ( -- )
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   AOT-ARM:W0 @ AOT-CAPTURE:PAYLOAD-TEST-PRIME ;

: MEMO-REUSE ( -- )
   PRIME-NEXT
   s" package PAYLOAD-FIRST public : ANCHOR ( -- ) ; ;package" DEFINITIONS
   END-BOUNDS
   s" PAYLOAD-FIRST" s" ANCHOR" AOT-CAPTURE:ACAP-MEMBER? TRUE!
   PRIME-NEXT
   s" package PAYLOAD-SECOND public : ANCHOR ( -- ) ; ;package" DEFINITIONS
   END-BOUNDS
   s" PAYLOAD-SECOND" s" ANCHOR" AOT-CAPTURE:ACAP-MEMBER? TRUE!
   s" PAYLOAD-FIRST" s" ANCHOR" AOT-CAPTURE:ACAP-MEMBER? 0= TRUE!
   s" payload memo reuse: ok" type cr ;

: BOUNDS ( n -- n n n n n n ) {: mode:n :}
   AOT-ARM:B0 @ mode 1 = if 1+ then
   AOT-ARM:B1 @ mode 2 = if 1+ then
   AOT-ARM:R0 @ mode 3 = if 1+ then
   AOT-ARM:R1 @ mode 4 = if 1+ then
   AOT-ARM:D0 @ mode 5 = if 1+ then
   AOT-ARM:D1 @ mode 6 = if 1+ then ;

public

: RUN ( n -- ) {: mode:n :}
   mode 0= if MEMO-REUSE exit then
   \ The retained compatibility host need not carry a new payload owner to test
   \ this earlier admission: every altered band must refuse before owner reads.
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   s" variable PAYLOAD-CELL : PAYLOAD-WORD ( n -- n ) 1+ ;" DEFINITIONS
   END-BOUNDS
   AOT-ARM:WINDOW$ AOT-CAPTURE:ACAP-PAYLOAD-BAND? TRUE!
   mode 7 = if s" payload matching band: ok" type cr exit then
   AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
   mode BOUNDS AOT-CAPTURE:CAPTURE
   s" payload admission accepted a mismatched band" 79 die ;

;package
0 SCRIPT-ARGV$ drop c@ $30 - PAYLOAD-ADMISSION-TEST:RUN
