\ code-reclaim.f - the live-XREF floor used when FORGET gives code back.

require lib/test.f
require src/habu/layout.f

package CRECL-TEST

private

TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

4 constant INSN-BYTES

: REC ( ptr u8 n -- ptr n )
   XREF-FIND
   dup XREF-FOUND? 0= if s" code-reclaim: subject not found" 76 die then ;

: REC-START ( ptr u8 n -- n )
   REC XREF-START ;

: REC-INDEX ( ptr u8 n -- n )
   XREF-FIND-INDEX ;

: DEFINED? ( ptr u8 n -- bool )
   XREF-FIND XREF-FOUND? ;

: FILL ( -- )
   s" : CR-F1 ( n -- n ) dup 3 * swap 7 + + ;" EV
   s" : CR-F2 ( n -- n ) dup 5 * swap 9 + + ;" EV
   s" : CR-F3 ( n -- n ) dup 11 * swap 13 + + ;" EV
   s" : CR-F4 ( n -- n ) dup 17 * swap 19 + + ;" EV ;

: FILL-FORGET ( -- )
   s" CR-F1" FORGET-DEFS-FROM ;

variable A-CP

: ALIAS-CASE ( -- )
   s" the alias record names the earlier routine" T-LABEL
   s" CRECL-ALIAS:CR-LOW" REC-START  s" CRECL-SUBJ:CR-LOW" REC-START  T=

   s" and follows the word compiled above that routine" T-LABEL
   s" CRECL-ALIAS:CR-LOW" REC-INDEX  s" CRECL-SUBJ:CR-HIGH" REC-INDEX  >  TTRUE
   s" CRECL-SUBJ:CR-HIGH" REC-START  s" CRECL-SUBJ:CR-LOW" REC-START  >  TTRUE

   cp@ A-CP !
   s" CRECL-ALIAS:CR-LOW" FORGET-DEFS-FROM

   s" forgetting the alias retires only its record" T-LABEL
   s" CRECL-ALIAS:CR-LOW" DEFINED? TFALSE
   cp@ A-CP @ T=

   FILL
   s" both surviving records keep their code" T-LABEL
   s" CRECL-SUBJ:CR-LOW" EV-N 11 T=
   s" CRECL-SUBJ:CR-HIGH" EV-N 22 T=
   FILL-FORGET ;

variable P-START

: PLAIN-CASE ( -- )
   s" : CR-P1 ( -- n ) 1 ;" EV
   s" : CR-P2 ( -- n ) 2 ;" EV
   s" : CR-P3 ( -- n ) 3 ;" EV
   s" CR-P2" REC-START P-START !
   s" CR-P2" FORGET-DEFS-FROM

   s" an ordinary forget returns the first retired routine's slot" T-LABEL
   cp@ P-START @ T=
   s" CR-P2" DEFINED? TFALSE
   s" CR-P3" DEFINED? TFALSE
   s" CR-P1" DEFINED? TTRUE
   s" CR-P1" EV-N 1 T= ;

variable G-CP

: REFUSE-BODY ( -- )
   s" CR-P1" REC-START CODE-RECLAIM:TRUNCATE ;

: REFUSE-CASE ( -- )
   cp@ G-CP !

   s" a floor at a surviving routine is refused" T-LABEL
   [: REFUSE-BODY ;] CODE-RECLAIM:E-LIVE TTHROWSQ

   s" a floor above the free slot is refused" T-LABEL
   [: cp@ INSN-BYTES + CODE-RECLAIM:TRUNCATE ;] CODE-RECLAIM:E-FLOOR TTHROWSQ

   s" refusals leave the pointer and live routine unchanged" T-LABEL
   cp@ G-CP @ T=
   s" CR-P1" EV-N 1 T= ;

variable U-START
variable U-INDEX

: REUSE-CASE ( -- )
   s" : CR-REUSE ( n -- n ) 1 + ;" EV
   s" CR-REUSE" REC-START U-START !
   s" CR-REUSE" REC-INDEX U-INDEX !
   s" CR-REUSE" FORGET-DEFS-FROM
   s" : CR-REUSE ( n -- n ) 3 * ;" EV

   s" a new definition reuses the reclaimed record and code slot" T-LABEL
   s" CR-REUSE" REC-START U-START @ T=
   s" CR-REUSE" REC-INDEX U-INDEX @ T=

   s" a caller compiled after reuse reaches the new bytes" T-LABEL
   s" : CR-REUSE-CALL ( n -- n ) CR-REUSE ;" EV
   s" 5 CR-REUSE-CALL" EV-N 15 T= ;

public

: RUN ( -- )
   T-RESET
   ALIAS-CASE
   PLAIN-CASE
   REFUSE-CASE
   REUSE-CASE
   T-REPORT ;

;package

package CRECL-SUBJ

public

: CR-LOW ( -- n )
   11 ;

: CR-HIGH ( -- n )
   22 ;

;package

package CRECL-ALIAS

public

EXPORT CRECL-SUBJ:CR-LOW

;package

CRECL-TEST:RUN
