\ postpone-effect.f - an effect a definer builds renders as the signature its
\ built word records, naming a private family by its package, and its nodes
\ are transient: no persistent schema root takes one. A multi-error load that
\ refuses a built body keeps its effect, which names the definer's private
\ structure, for a caller in another package. The file calls the running
\ checker's words, so it is loaded, never built.

require src/lib/test.f

package POSTPONE-CELL
private
STRUCTURE pdcell FIELD v n ;STRUCTURE
1 TYPED-BUFFER PD-CELLS pdcell

\ ( ptr pdcell -- ptr pdcell ), a type no other package can spell.
: CELL-EFFECT ( -- EFFECT:type )
   s" POSTPONE-CELL:pdcell" EFFECT:FAMILY EFFECT:PTR
   {: p :}  EFFECT:NEW p EFFECT:IN  p EFFECT:OUT ;

public
: PD-CELL ( -- ptr pdcell ) 0 PD-CELLS ;

\ : NAME ( ptr pdcell -- ptr pdcell ) drop drop ;, an underflow.
: MAKE-CELL-BROKEN ( ptr u8 n -- )
   CELL-EFFECT define-word
   postpone drop  postpone drop
   postpone ; ;
;package

package POSTPONE-EFFECT
private
STRUCTURE peslot FIELD a n ;STRUCTURE
CAST: >NODE ( EFFECT:type -- n )

: SLOT-PTR ( -- EFFECT:type )
   s" POSTPONE-EFFECT:peslot" EFFECT:FAMILY EFFECT:PTR ;

\ ( ptr peslot n -- ptr peslot ), one pointer node in both rows.
: SLOT-EFFECT ( -- EFFECT:type )
   SLOT-PTR {: p :}
   EFFECT:NEW p EFFECT:IN  EFFECT:N EFFECT:IN  p EFFECT:OUT ;

\ The text a built word records as its signature.
: SLOT-TEXT ( -- ptr u8 n ) SLOT-EFFECT >NODE CHECK:EFFECT-TEXT ;

SLOT-TEXT s" ( ptr postpone-effect:peslot n -- ptr postpone-effect:peslot )" T$=

: ROOT-PERSIST ( -- )
   SLOT-PTR >NODE {: node:n :}
   [: node SCHEMA-REG:SCHEMA-ROOT+ drop ;] SCHEMA-REG:E-SCHEMA-BAD TTHROWSQ ;
ROOT-PERSIST

\ Nor does a persistent pointer node.
: PTR-PERSIST ( -- )
   SLOT-PTR >NODE {: node:n :}
   [: node SCHEMA-REG:SCHEMA-PTR drop ;] SCHEMA-REG:E-SCHEMA-BAD TTHROWSQ ;
PTR-PERSIST

TYPED-VARIABLE EVAL-A ptr u8
variable EVAL-U
: EVAL-RUN ( -- ) EVAL-A @ EVAL-U @ INCLUDE-EVALUATE ;
\ The refusals counted and the throw code when the text is evaluated under the
\ multi-error load state.
: MULTI ( ptr u8 n -- n n )
   EVAL-U ! EVAL-A !  MULTI-ERR-BEGIN [: EVAL-RUN ;] catch MULTI-ERR-END ;

\ The refused built word keeps its effect: its caller certifies, one refusal.
S\" s\" CELL-BROKEN\" POSTPONE-CELL:MAKE-CELL-BROKEN\n: CELL-USE ( -- ) POSTPONE-CELL:PD-CELL CELL-BROKEN drop ;"
   MULTI 1 T= 0 T=
;package

T-REPORT
