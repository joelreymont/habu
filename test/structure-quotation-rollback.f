\ A caught declaration with an unfinished quotation must leave the parser
\ usable for the next declaration, including nested quotations and SUMTYPE.
require src/lib/test.f

package QUOT-ROLLBACK-TEST

: RUN ( -- )
   T-RESET
   64 0 ?do
      s" STRUCTURE qfail FIELD handler [ n n n n nope -- ] ;STRUCTURE"
         TEST-EVAL:RC 7109 T=
   loop
   s" STRUCTURE qgood FIELD handler [ n -- n ] ;STRUCTURE" TEST-EVAL:RC 0 T=
   s" STRUCTURE qnest FIELD handler [ [ n -- n ] nope -- ] ;STRUCTURE"
      TEST-EVAL:RC 7109 T=
   s" STRUCTURE qnest FIELD handler [ [ n -- n ] -- ] ;STRUCTURE"
      TEST-EVAL:RC 0 T=
   s" SUMTYPE qbad VARIANT call [ n nope -- n ] ;VARIANT ;SUMTYPE"
      TEST-EVAL:RC 7109 T=
   s" SUMTYPE qsum VARIANT call [ n -- n ] ;VARIANT ;SUMTYPE"
      TEST-EVAL:RC 0 T=
   T-REPORT ;

RUN
;package
