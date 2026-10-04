\ A caught declaration with an unfinished quotation must leave the parser
\ usable for the next declaration, including nested quotations and SUMTYPE.
require lib/test.f

package QUOT-ROLLBACK-TEST

\ TRY evaluates a copy of the string because a throw restores the depth catch
\ began with, and it drops both copies so a refusal leaves only its code.
: TRY ( ptr u8 n -- n ) [: 2dup evaluate-closed ;] catch {: rc:n :} 2drop rc ;

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
