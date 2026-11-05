\ Initialized record queries use committed field metadata from real declarations.
\ Run on the unsealed candidate through the WHITEBOX-SUITE load path.

require src/lib/test.f

package C2-INIT-RECORD-SCHEMA

public
PRODUCT pair FIELD left n FIELD right n ;PRODUCT
STRUCTURE flexible<a> FIELD first a FIELD mark n ;STRUCTURE
STRUCTURE inner-flex<a> FIELD value a ;STRUCTURE
STRUCTURE outer-flex<a> FIELD nested inner-flex<a> ;STRUCTURE

private

: FAMILY ( ptr u8 n -- n ) {: name:ptr size:n :}
   s" c2-init-record-schema" name size TFAM:TFAM-RESOLVE drop ;

: TERM ( ptr u8 n -- n )
   FAMILY {: fam:n :}
   PARAM-SCR-N @ fam TFAM-NAME$ fam MK-PARAM ;

: APPLY ( n ptr u8 n -- n ) {: arg:n name:ptr size:n :}
   name size FAMILY {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   arg PARAM-SCR+
   base fam TFAM-NAME$ fam MK-PARAM ;

: OPEN ( -- n ) NEW FRESH MK-VAR ;


public

: CHECK ( -- )
   s" a committed two-cell product has a fixed schema" T-LABEL
   s" pair" TERM TFAM:INIT-RECORD? TTRUE CELL T= 2 CELL * T= 2 T=
   s" an open width-bearing field is not a fixed schema" T-LABEL
   OPEN s" flexible" APPLY TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T=
   OPEN s" outer-flex" APPLY TFAM:INIT-RECORD? TFALSE 0 T= 0 T= 0 T= ;

;package

T-RESET
C2-INIT-RECORD-SCHEMA:CHECK
T-REPORT
s" c2-init-record-schema: ok" type cr
