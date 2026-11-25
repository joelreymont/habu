\ A caught declaration with an unfinished quotation must leave the parser
\ usable for the next declaration.
require src/lib/test.f
require test/decl-diag-capture.f

package QUOT-ROLLBACK-TEST

\ REJECTS asserts the text stops with the checker's status after a declaration
\ record whose evaluated note marks `nope` under the label.
: REJECTS ( str str -- )
   {: sa:str la:str :}
   REJECTION:CAPTURE  sa TEST-EVAL:RC 70 T=
   REJECTION:BUFFER$ s" error[E-BAD-DECLARATION]" STR:CONTAINS? TTRUE
   s" nope" la DECL-DIAG:CAUSE? TTRUE
   REJECTION:BUFFER-OFF ;

: RUN ( -- )
   T-RESET
   64 0 ?do
      s" STRUCTURE qfail FIELD handler [ n n n n nope -- ] ;STRUCTURE"
         s" unknown type" REJECTS
   loop
   s" STRUCTURE qgood FIELD handler [ n -- n ] ;STRUCTURE" TEST-EVAL:RC 0 T=
   s" STRUCTURE qnest FIELD handler [ [ n -- n ] nope -- ] ;STRUCTURE"
      s" unknown type" REJECTS
   s" STRUCTURE qnest FIELD handler [ [ n -- n ] -- ] ;STRUCTURE"
      TEST-EVAL:RC 0 T=
   s" SUMTYPE qbad VARIANT call [ n nope -- n ] ;VARIANT ;SUMTYPE"
      s" unknown payload type" REJECTS
   s" SUMTYPE qsum VARIANT call [ n -- n ] ;VARIANT ;SUMTYPE"
      TEST-EVAL:RC 0 T=
   T-REPORT ;

RUN
;package
