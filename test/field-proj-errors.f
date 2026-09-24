\ Only an absent committed field is a projection miss. A fault while computing
\ its instantiated width must reach the caller, even if it also uses E-PF-ID.
require lib/test.f

package FIELD-PROJ-ERRORS
public
STRUCTURE inner 0 FIELD value n ;STRUCTURE
STRUCTURE outer 1 FIELD value a ;STRUCTURE
private

variable INNER-FAM
variable OUTER-FAM
variable FIELD-ID
variable ERROR

TRUSTED: FIND-FAM ( ptr u8 n -- n )
   s" FIELD-PROJ-ERRORS" 2swap TFAM:TFAM-FIND-IN drop ;

TRUSTED: TERM ( -- n )
   PARAM-SCR-N @ {: base:n :}
   base s" inner" INNER-FAM @ MK-PARAM PARAM-SCR+
   base s" outer" OUTER-FAM @ MK-PARAM ;

: PROJECT ( -- ) FIELD-ID @ 0 TERM FIELD-PROJ-XT 2drop ;
: FAIL-WIDTH ( n -- n ) drop ERROR @ throw ;
: BREAK-WIDTH ( -- ) ['] FAIL-WIDTH is TFAM-INST-WIDTH-XT ;

: CASES ( -- )
   s" inner" FIND-FAM INNER-FAM !
   s" outer" FIND-FAM OUTER-FAM !
   OUTER-FAM @ TYPE-FIELD:NO-VARIANT s" value" TYPE-FIELD:FIND
   TTRUE FIELD-ID !
   FIELD-ID @ 0 TERM FIELD-PROJ-XT TTRUE drop
   -1 0 TERM FIELD-PROJ-XT TFALSE 0 T=
   BREAK-WIDTH
   TFAM:E-PF-TX ERROR !
   [: PROJECT ;] TFAM:E-PF-TX TTHROWSQ
   TFAM:E-PF-ID ERROR !
   [: PROJECT ;] TFAM:E-PF-ID TTHROWSQ ;

: RUN ( -- )
   T-RESET
   CASES
   T-REPORT ;

\ This suite owns its whitebox process; compile every definition before CASES
\ installs the failing width hook.
RUN
;package
