\ postpone-effect.f - an effect a definer builds renders as the signature its
\ built word records, naming a private family by its package, and its nodes
\ are transient: no persistent schema root takes one. A multi-error load that
\ refuses a built body keeps its effect, which names the definer's
\ structure, for a caller in another package. A word built over
\ EFFECT:READ-TYPE or EFFECT:NAMED of a type takes the type its text form
\ declares, and READ-TYPE refuses a family the running package does not know.
\ The file calls the running checker's words, so it is loaded, never built.

require src/lib/test.f
require src/lib/adt/nullable.f

package POSTPONE-CELL
public
STRUCTURE pdcell FIELD v n ;STRUCTURE
private
STRUCTURE pdhidden FIELD v n ;STRUCTURE
1 TYPED-BUFFER PD-CELLS pdcell

\ ( ptr pdcell -- ptr pdcell ).
: CELL-EFFECT ( -- EFFECT:type )
   s" POSTPONE-CELL:pdcell" EFFECT:NAMED EFFECT:PTR
   {: p :}  EFFECT:EMPTY p EFFECT:IN  p EFFECT:OUT ;

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
   s" POSTPONE-EFFECT:peslot" EFFECT:NAMED EFFECT:PTR ;

\ ( ptr peslot n -- ptr peslot ), one pointer node in both rows.
: SLOT-EFFECT ( -- EFFECT:type )
   SLOT-PTR {: p :}
   EFFECT:EMPTY p EFFECT:IN  EFFECT:N EFFECT:IN  p EFFECT:OUT ;

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

1 TYPED-BUFFER PE-SLOTS peslot
TYPED-VARIABLE PE-FLAG bool
CAST: N>R ( n -- r )
CAST: R>N ( r -- n )

\ : NAME ( t -- t ) ;
: MAKE-ID ( ptr u8 n EFFECT:type -- ) {: t :}
   EFFECT:EMPTY t EFFECT:IN  t EFFECT:OUT  define-word
   postpone ; ;

s" PE-U32" s" u32" EFFECT:READ-TYPE MAKE-ID
s" PE-BYTES" s" ptr u8" EFFECT:READ-TYPE MAKE-ID
s" PE-NUL" s" nullable<u8>" EFFECT:READ-TYPE MAKE-ID
s" PE-SLOT" s" ptr peslot" EFFECT:READ-TYPE MAKE-ID
s" PE-R" s" r" EFFECT:NAMED MAKE-ID
s" PE-BOOL" s" bool" EFFECT:NAMED MAKE-ID
s" PE-U8" s" u8" EFFECT:NAMED MAKE-ID
s" PE-N" s" n" EFFECT:NAMED MAKE-ID

: PE-NUL-BYTE ( nullable<u8> -- n )
   MATCH nullable
      none OF 0 ENDOF
      some OF c@ ENDOF
   ;MATCH ;

7 PE-U32 7 T=
s" : PE-U32-NARROW ( u32 -- u8 ) PE-U32 ;" TEST-EVAL:RC 70 T=
s" ab" drop PE-BYTES c@ 97 T=
s" 0 PE-SLOTS PE-BYTES drop" TEST-EVAL:RC 70 T=
s" Q" drop NULLABLE:SOME PE-NUL PE-NUL-BYTE 81 T=
s" PE-FLAG NULLABLE:SOME PE-NUL drop" TEST-EVAL:RC 70 T=
0 PE-SLOTS PE-SLOT NULL-PTR -  0 PE-SLOTS NULL-PTR -  T=
s" POSTPONE-CELL:PD-CELL PE-SLOT drop" TEST-EVAL:RC 70 T=
5 N>R PE-R R>N 5 T=
s" 5 PE-R drop" TEST-EVAL:RC 70 T=
true PE-BOOL TTRUE
s" 5 PE-BOOL drop" TEST-EVAL:RC 70 T=
65 PE-U8 65 T=
s" : PE-U8-WIDE ( -- n ) 300 PE-U8 ;" TEST-EVAL:RC 70 T=
5 PE-N 5 T=
s" true PE-N drop" TEST-EVAL:RC 70 T=

\ POSTPONE-CELL's private family is unknown here.
: READ-HIDDEN ( -- n ) [: s" pdhidden" EFFECT:READ-TYPE drop ;] catch ;
READ-HIDDEN 70 T=
;package

T-REPORT
