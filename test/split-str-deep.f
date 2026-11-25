split-str
\ split-str-deep.f - included by test/split-str-test.f: under the marker a
\ signature nested past EFFECT-DEPTH-MAX is rejected by that limit, as it is
\ unmarked, and one at the limit splits. A rejection ends the load, so each
\ text runs in a child that inherits the marker.

require src/lib/test/assert.f

package SPLIT-STR-DEEP

$10000 constant CAP            \ bytes: the deepest text
create SRC-BUF CAP allot
create OUT CAP allot
create ERR CAP allot
TYPED-VARIABLE SRC-U len

: SRC-RESET ( -- )   SRC-U BUF-RESET ;
: S+ ( str -- )   SRC-BUF CAP SRC-U BUF-APPEND ;
: SRC$ ( -- ^u8 n )   SRC-BUF SRC-U @ LEN>N ;
: S+N ( str n -- ) {: a:^ u:n k:n :}   k 0 ?do a u S+ loop ;

\ The input of a quotation nested k deep, each the input of the next.
: QUOT-INPUT ( n -- ) {: k:n :}
   s" [ " k S+N  s" n -- ]" S+  s"  -- ]" k 1 - S+N ;

\ K quotations, each the input of the next: 2k levels deep.
: QUOT-NEST ( n -- ) {: k:n :}
   s" [ " k S+N  s" -- ]" S+  s"  -- ]" k 1 - S+N ;

\ A colon definition whose input quotation nests k deep.
: DEF-QUOT ( n -- ) {: k:n :}
   SRC-RESET  s" : SPD-Q ( " S+  k QUOT-NEST  s"  -- ) drop ;" S+ ;

\ A colon definition whose row holds k entries in and out.
: DEF-ROW ( n -- ) {: k:n :}
   SRC-RESET  s" : SPD-R ( " S+  s" n " k S+N  s" -- " S+  s" n " k S+N  s" ) ;" S+ ;

\ A word whose input quotation nests k deep, called by a word with that input.
: QUOT-CALL ( n -- ) {: k:n :}
   SRC-RESET
   s" : SPD-QK ( " S+  k QUOT-INPUT  s"  -- ) drop ; " S+
   s" : SPD-QGO ( " S+  k QUOT-INPUT  s"  -- ) SPD-QK ;" S+ ;

\ A colon definition whose signature opens k quotations it never closes.
: DEF-OPEN ( n -- ) {: k:n :}
   SRC-RESET  s" : SPD-OPEN ( " S+  s" [" k S+N  s"  -- ) ;" S+ ;

\ A colon definition whose local's type opens k quotations it never closes.
: LOCAL-OPEN ( n -- ) {: k:n :}
   SRC-RESET  s" : SPD-LOCAL ( n -- ) {: x:" S+  s" [" k S+N  s"  :} ;" S+ ;

: DEFER-OPEN ( -- )   SRC-RESET  s" defer SPD-DEFER ( " S+ ;
: DEFER-CLOSE ( -- )   s"  -- )" S+ ;

\ A stored row whose input is k nested pointees.
: POINTEES ( n -- ) {: k:n :}
   DEFER-OPEN  s" ^" k S+N  s" n" S+  DEFER-CLOSE ;

\ A stored row whose input is k nested quotations, each the input of the next.
: QUOTATIONS ( n -- ) {: k:n :}
   DEFER-OPEN  k QUOT-NEST  DEFER-CLOSE ;

\ The text in SRC-BUF, run as the subject, exits 70 and names why on stderr.
: REJECTS ( ^u8 n -- ) {: why:^ whyu:n :}
   SRC$ OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   SRC$ OUT outu LEN>N ERR erru LEN>N oc 70 T-ENDING-EXITED=
   ERR erru LEN>N why whyu STR:CONTAINS? TTRUE ;

: LOADS ( -- )
   SRC$ OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   SRC$ OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED= ;

public

: RUN ( -- )
   s" a definition's signature nested past the depth limit rejects by it" T-LABEL
   1025 DEF-OPEN
   s" effect too deep to record (more than 2048 levels)" REJECTS
   s" a local's type nested past the depth limit rejects by it" T-LABEL
   1025 LOCAL-OPEN
   s" effect too deep to record (more than 2048 levels)" REJECTS
   s" 9000 nested pointees reject by the depth limit" T-LABEL
   9000 POINTEES
   s" effect too deep to record (more than 2048 levels)" REJECTS
   s" 4500 nested quotations reject by the depth limit" T-LABEL
   4500 QUOTATIONS
   s" effect too deep to record (more than 2048 levels)" REJECTS
   s" a definition's input quotation nested 1024 deep splits" T-LABEL
   1024 DEF-QUOT  LOADS
   s" a definition's row of 2048 entries splits" T-LABEL
   2048 DEF-ROW  LOADS
   s" a call of a word whose input quotation nests 1023 deep splits" T-LABEL
   1023 QUOT-CALL  LOADS ;

;package

SPLIT-STR-DEEP:RUN
