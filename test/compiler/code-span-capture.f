\ Namespace WIDs are never decoded as code lengths during a real capture.
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

AOT-ARM:WINDOW-OPEN
package SPAN-CAPTURE-A
private
: HIDDEN ( -- ) ;
;package
package SPAN-CAPTURE-B
private
: HIDDEN ( -- ) ;
;package
AOT-ARM:WINDOW-CLOSE

package AOT-CAPTURE
using AOT-BUF

create SPAN-BAD-REC 0 , 3 , 0 , 0 , 0 , 0 ,
create SPAN-OUT $100 allot
create SPAN-ERR $100 allot

: SPAN-NAMESPACE ( -- )
   0
   AOT-ARM:R1 @ AOT-ARM:R0 @ ?do
      i AOT-REC {: rec:ptr :}
      rec AOT-RWID DICT-WL:NAMESPACE = if
         rec AOT-RLEN 3 and 0<> if 1+ then
      then
   loop
   0 > TTRUE
   AOT-ARM:R0 @ AOT-ARM:D0 @ PRELUDE-MARK
   AOT-ARM:WINDOW$ CAPTURE
   AOT-REC-N @ 0 > TTRUE ;

: SPAN-MALFORMED ( -- )
   s" package AOT-CAPTURE SPAN-BAD-REC CELL-VIEW AOT-RBODY drop ;package"
   SPAN-OUT $100 >LEN SPAN-ERR $100 >LEN 1000 >MS SUBJECT:RUN
   74 T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   SPAN-ERR erru LEN>N s" hb: malformed dictionary code length" T$= ;

: SPAN-CAPTURE-RUN ( -- )
   T-RESET
   s" capture skips namespace private WIDs that are not code aligned" T-LABEL
   SPAN-NAMESPACE
   s" ordinary malformed code lengths still refuse" T-LABEL
   SPAN-MALFORMED
   T-REPORT ;

' SPAN-CAPTURE-RUN
;using
;package
execute
