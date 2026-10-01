\ A caught declaration with an unfinished quotation must leave the parser
\ usable for the next declaration, including nested quotations and SUMTYPE.
require lib/test.f

package QUOT-ROLLBACK-TEST

TRUSTED: DECL ( ptr u8 n -- ) evaluate ;
TRUSTED: TRY ( ptr u8 n -- n ) ['] DECL catch ;

: RUN ( -- )
   T-RESET
   64 0 ?do
      s" STRUCTURE qfail 0 FIELD handler [ n n n n nope -- ] ;STRUCTURE"
         TRY 7109 T=
   loop
   s" STRUCTURE qgood 0 FIELD handler [ n -- n ] ;STRUCTURE" TRY 0 T=
   s" STRUCTURE qnest 0 FIELD handler [ [ n -- n ] nope -- ] ;STRUCTURE"
      TRY 7109 T=
   s" STRUCTURE qnest 0 FIELD handler [ [ n -- n ] -- ] ;STRUCTURE"
      TRY 0 T=
   s" SUMTYPE qbad 0 VARIANT call [ n nope -- n ] ;VARIANT ;SUMTYPE"
      TRY 7109 T=
   s" SUMTYPE qsum 0 VARIANT call [ n -- n ] ;VARIANT ;SUMTYPE"
      TRY 0 T=
   T-REPORT ;

RUN
;package
