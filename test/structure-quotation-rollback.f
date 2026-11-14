\ A caught declaration with an unfinished quotation must leave the parser
\ usable for the next declaration.
require src/lib/test.f

package QUOT-ROLLBACK-TEST

\ REJECTS asserts the text stops with the checker's status after a declaration
\ record that names `nope` as the unknown type.
: REJECTS ( ptr u8 n -- )
   REJECTION:CAPTURE  TEST-EVAL:RC CHECKER-REJECT-RC T=
   REJECTION:BUFFER$ s" error[E-BAD-DECLARATION]" CONTAINS? TTRUE
   REJECTION:BUFFER$ s" at `nope`: unknown" CONTAINS? TTRUE
   REJECTION:BUFFER-OFF ;

: RUN ( -- )
   T-RESET
   64 0 ?do
      s" STRUCTURE qfail FIELD handler [ n n n n nope -- ] ;STRUCTURE" REJECTS
   loop
   s" STRUCTURE qgood FIELD handler [ n -- n ] ;STRUCTURE" TEST-EVAL:RC 0 T=
   s" STRUCTURE qnest FIELD handler [ [ n -- n ] nope -- ] ;STRUCTURE" REJECTS
   s" STRUCTURE qnest FIELD handler [ [ n -- n ] -- ] ;STRUCTURE"
      TEST-EVAL:RC 0 T=
   s" SUMTYPE qbad VARIANT call [ n nope -- n ] ;VARIANT ;SUMTYPE" REJECTS
   s" SUMTYPE qsum VARIANT call [ n -- n ] ;VARIANT ;SUMTYPE"
      TEST-EVAL:RC 0 T=
   T-REPORT ;

RUN
;package
