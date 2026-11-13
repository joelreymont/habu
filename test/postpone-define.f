\ postpone-define.f - a definer builds a definition with define-word,
\ body-append, literal and postpone, and each postponed word binds the word
\ the definer found when it was compiled, whatever the building package
\ defines: a global, the definer package's private word and its public one.
\ The same body evaluated as text binds the building package's words, and
\ body-append or literal with no definition open is refused. A STRUCTURE's AT
\ answers its element in a package with its own `*` and `record-at`. The file
\ is also a build source: MAIN prints the four answers.

require src/lib/test.f

package POSTPONE-DEFINER
private
: BUMP ( n -- n ) 1 + ;
public
: DOUBLE ( n -- n ) 2 * ;

\ : NAME ( n -- n ) 3 * ;
: MAKE-TRIPLER ( ptr u8 n -- )
   s" ( n -- n )" define-word
   s" 3" body-append
   postpone *
   postpone ; ;

\ : NAME ( n -- n ) BUMP DOUBLE ;
: MAKE-STEPPER ( ptr u8 n -- )
   s" ( n -- n )" define-word
   postpone BUMP
   postpone DOUBLE
   postpone ; ;

\ : NAME ( -- n ) -6 7 * ;
: MAKE-NEG-ANSWER ( ptr u8 n -- )
   s" ( -- n )" define-word
   -6 literal  7 literal
   postpone *
   postpone ; ;
;package

package POSTPONE-USER
private
: * ( n n -- n ) + ;
: BUMP ( n -- n ) 100 + ;
: DOUBLE ( n -- n ) drop 0 ;

s" TRIPLE" POSTPONE-DEFINER:MAKE-TRIPLER
s" STEP" POSTPONE-DEFINER:MAKE-STEPPER
s" NEG-ANSWER" POSTPONE-DEFINER:MAKE-NEG-ANSWER
s" : TRIPLE-TEXT ( n -- n ) 3 * ;" evaluate-closed

7 TRIPLE 21 T=
7 STEP 16 T=
7 TRIPLE-TEXT 10 T=
NEG-ANSWER -42 T=

: APPEND-NONE ( -- n ) [: s" 1" body-append ;] catch ;
APPEND-NONE 0 T<>
: LITERAL-NONE ( -- n ) [: 1 literal ;] catch ;
LITERAL-NONE 70 T=

public
: ANSWERS ( -- ) 7 TRIPLE . 7 STEP . 7 TRIPLE-TEXT . NEG-ANSWER . cr ;
;package

package POSTPONE-AT
private
: * ( n n -- n ) + ;
: record-at ( ptr n n -- ptr n ) drop ;
STRUCTURE pdpair DERIVE addr FIELD a n FIELD b n ;STRUCTURE
3 TYPED-BUFFER PD-PAIRS pdpair
\ AT's stride, measured from the buffer's element 0.
: PD-STRIDE ( -- n )
   0 PD-PAIRS {: p:ptr :}
   p 1 PDPAIR-AT NULL-PTR -  p NULL-PTR - - ;
PD-STRIDE PDPAIR-BYTES T=
;package

\ An effect built from found types renders as the text a definition would
\ declare, and its nodes are transient: no persistent schema root takes one.
package POSTPONE-EFFECT
private
STRUCTURE peslot FIELD a n ;STRUCTURE
CAST: >NODE ( EFFECT:type -- n )

: SLOT-FAM ( -- n ) [package] s" peslot" TFAM:TFAM-FIND-IN drop ;

: SLOT-PTR ( -- EFFECT:type ) SLOT-FAM EFFECT:FAMILY EFFECT:PTR ;

\ ( ptr peslot n -- ptr peslot ), one pointer node in both rows.
: SLOT-EFFECT ( -- EFFECT:type )
   SLOT-PTR {: p :}
   EFFECT:NEW p EFFECT:IN  EFFECT:N EFFECT:IN  p EFFECT:OUT ;

\ peslot has no parameters, so its own term instantiates the effect.
: SLOT-TERM ( -- n )
   SLOT-FAM {: fam:n :}
   PARAM-SCR-N @ fam TFAM-NAME$ fam MK-PARAM
   SLOT-EFFECT >NODE TFAM:SCHEMA-TERM-FOR ;

: SLOT-TEXT ( -- ptr u8 n )
   SLOT-TERM {: q:n :}
   0 RSN !  q Q>DIN q Q>DOUT 0 0 false REND-FACTS ;

SLOT-TEXT s" ( ptr peslot n -- ptr peslot ) " T$=

: ROOT-PERSIST ( -- )
   SLOT-PTR >NODE {: node:n :}
   [: node SCHEMA-REG:SCHEMA-ROOT+ drop ;] SCHEMA-REG:E-SCHEMA-BAD TTHROWSQ ;
ROOT-PERSIST

\ Nor does a persistent pointer node.
: PTR-PERSIST ( -- )
   SLOT-PTR >NODE {: node:n :}
   [: node SCHEMA-REG:SCHEMA-PTR drop ;] SCHEMA-REG:E-SCHEMA-BAD TTHROWSQ ;
PTR-PERSIST
;package

: MAIN ( -- ) POSTPONE-USER:ANSWERS ;
