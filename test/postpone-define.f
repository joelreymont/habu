\ postpone-define.f - a definer builds a definition with define-word,
\ literal, compile, and postpone, and each postponed or compiled word binds
\ the word the definer found when it was compiled, whatever the building
\ package defines: a global, the definer package's private word and its public
\ one. The same body evaluated as text binds the building package's words,
\ and literal or compile, with no definition open is refused. define-word
\ takes the effect a definer built, a parametric one included, and refuses a
\ type that is not an effect. A definition the checker refuses at the
\ definer's postpone ; is abandoned, and the next definition compiles. An
\ effect holds at most 23 type parameters, numbered per effect however many
\ the file made before. A STRUCTURE's AT answers its element in a package
\ with its own `*` and `record-at`. The file is also a build source: MAIN
\ prints the four answers.

require src/lib/test.f
require src/lib/adt/nullable.f

package POSTPONE-DEFINER
private
: BUMP ( n -- n ) 1 + ;
public
: DOUBLE ( n -- n ) 2 * ;

private
\ ( n -- n )
: N>N ( -- EFFECT:type ) EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT ;

public
\ nullable on a parameter no other value holds.
: NULLABLE-T ( -- EFFECT:type )
   s" nullable" EFFECT:NAMED ;

\ : NAME ( n -- n ) 3 * ;
: MAKE-TRIPLER ( ptr u8 n -- )
   N>N define-word
   3 literal
   postpone *
   postpone ; ;

\ : NAME ( n -- n ) BUMP DOUBLE ;
: MAKE-STEPPER ( ptr u8 n -- )
   N>N define-word
   postpone BUMP
   postpone DOUBLE
   postpone ; ;

\ MAKE-STEPPER through compile,.
: MAKE-STEPPER2 ( ptr u8 n -- )
   N>N define-word
   ['] BUMP compile,
   ['] DOUBLE compile,
   postpone ; ;

\ : NAME ( -- n ) -6 7 * ;
: MAKE-NEG-ANSWER ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   -6 literal  7 literal
   postpone *
   postpone ; ;

\ : NAME ( nullable<a> -- nullable<a> nullable<a> ) dup ; one type value in
\ all three places.
: MAKE-DUP ( ptr u8 n -- )
   NULLABLE-T {: t :}
   EFFECT:EMPTY t EFFECT:IN  t EFFECT:OUT  t EFFECT:OUT  define-word
   postpone dup
   postpone ; ;

\ : NAME ( nullable<a> nullable<b> ... -- ) drop drop ... ; K inputs.
: MAKE-WIDE ( ptr u8 n n -- ) {: k:n :}
   EFFECT:EMPTY  k 0 ?do NULLABLE-T EFFECT:IN loop  define-word
   k 0 ?do postpone drop loop
   postpone ; ;

\ These names start with the first byte of `c"` or `."`: a postponed name
\ binds the word whatever its spelling.
: CUBE ( n -- n ) dup dup * * ;
: .HALF ( n -- n ) 2 / ;

\ : NAME ( n -- n ) CUBE ;
: MAKE-CUBER ( ptr u8 n -- )
   N>N define-word
   postpone CUBE
   postpone ; ;

\ : NAME ( n -- n ) .HALF ;
: MAKE-HALVER ( ptr u8 n -- )
   N>N define-word
   postpone .HALF
   postpone ; ;

\ : NAME ( -- n ) drop ;, an underflow the checker rejects.
: MAKE-BROKEN ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   postpone drop
   postpone ; ;

\ : NAME ( -- n ) CUBE ;, an underflow the checker rejects.
: MAKE-SHORT-CUBER ( ptr u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   postpone CUBE
   postpone ; ;

\ : NAME ( -- read-view<a,b,c> ) 1 throw ;, a view no input supplies.
: MAKE-RV ( ptr u8 n -- )
   EFFECT:EMPTY s" read-view" EFFECT:NAMED EFFECT:OUT define-word
   1 literal
   postpone throw
   postpone ; ;
;package

package POSTPONE-USER
private
: * ( n n -- n ) + ;
: BUMP ( n -- n ) 100 + ;
: DOUBLE ( n -- n ) drop 0 ;

s" TRIPLE" POSTPONE-DEFINER:MAKE-TRIPLER
s" STEP" POSTPONE-DEFINER:MAKE-STEPPER
s" STEP2" POSTPONE-DEFINER:MAKE-STEPPER2
s" NEG-ANSWER" POSTPONE-DEFINER:MAKE-NEG-ANSWER
s" : TRIPLE-TEXT ( n -- n ) 3 * ;" evaluate-closed

7 TRIPLE 21 T=
7 STEP 16 T=
7 STEP2 16 T=
7 TRIPLE-TEXT 10 T=
NEG-ANSWER -42 T=

s" CUBED" POSTPONE-DEFINER:MAKE-CUBER
s" HALVED" POSTPONE-DEFINER:MAKE-HALVER
3 CUBED 27 T=
8 HALVED 4 T=

\ Each definer run's parameter is a new one, and each built word certifies as
\ parametric: a nullable<u8> passes through both.
s" DUP-A" POSTPONE-DEFINER:MAKE-DUP
s" DUP-B" POSTPONE-DEFINER:MAKE-DUP
: NUL-BYTE ( nullable<u8> -- n )
   MATCH nullable
      none OF 0 ENDOF
      some OF c@ ENDOF
   ;MATCH ;
: DUPS ( -- n )
   s" Q" drop NULLABLE:SOME DUP-A DUP-B
   NUL-BYTE swap NUL-BYTE + swap NUL-BYTE + ;
DUPS 243 T=

\ The 24 parameters the file made before a run leave that run's effect room.
: FAMILIES ( -- ) 24 0 ?do POSTPONE-DEFINER:NULLABLE-T drop loop ;
FAMILIES
s" DUP-C" POSTPONE-DEFINER:MAKE-DUP
: DUPS-C ( -- n ) s" Q" drop NULLABLE:SOME DUP-C NUL-BYTE swap NUL-BYTE + ;
DUPS-C 162 T=

\ Each rejection's code and place: a name no source holds is placed at the
\ word the file ran and named in the `word` note.
: HAS ( ptr u8 n -- ) {: a:ptr u:n :}   REJECTION:BUFFER$ a u CONTAINS? TTRUE ;

: NOT-EFFECT ( -- n ) [: s" NOT-BUILT" EFFECT:N define-word ;] catch ;
REJECTION:CAPTURE
NOT-EFFECT 70 T=
s" error[E-NOT-EFFECT]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" NOT-EFFECT 70 T=" T-PLACED
s" = word: NOT-BUILT" HAS  REJECTION:BUFFER-OFF

\ 23 type parameters fit an effect, a 24th is E-NOT-EFFECT.
: WIDE ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   [: a u k POSTPONE-DEFINER:MAKE-WIDE ;] catch ;
s" WIDE-23" 23 WIDE 0 T=
REJECTION:CAPTURE
s" WIDE-24" 24 WIDE 70 T=
s" error[E-NOT-EFFECT]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 16 s" 24 WIDE 70 T=" T-PLACED
s" = word: WIDE-24" HAS  REJECTION:BUFFER-OFF

\ A spelling that names no family is E-UNKNOWN-SIGNATURE-TYPE.
: NO-FAMILY ( -- n ) [: s" no-such-family" EFFECT:NAMED drop ;] catch ;
REJECTION:CAPTURE
NO-FAMILY 70 T=
s" error[E-UNKNOWN-SIGNATURE-TYPE]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" NO-FAMILY 70 T=" T-PLACED
REJECTION:BUFFER-OFF

\ EFFECT:IN given a type that is no effect is E-NOT-EFFECT.
: IN-NOT-EFFECT ( -- n ) [: EFFECT:N EFFECT:N EFFECT:IN drop ;] catch ;
REJECTION:CAPTURE
IN-NOT-EFFECT 70 T=
s" error[E-NOT-EFFECT]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" IN-NOT-EFFECT 70 T=" T-PLACED
REJECTION:BUFFER-OFF

: LITERAL-NONE ( -- n ) [: 1 literal ;] catch ;
REJECTION:CAPTURE
LITERAL-NONE 70 T=
s" error[E-NO-DEFINITION]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" LITERAL-NONE 70 T=" T-PLACED
REJECTION:BUFFER-OFF
: COMPILE-NONE ( -- n ) [: ['] DOUBLE compile, ;] catch ;
REJECTION:CAPTURE
COMPILE-NONE 70 T=
s" error[E-NO-DEFINITION]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" COMPILE-NONE 70 T=" T-PLACED
REJECTION:BUFFER-OFF

: BROKEN ( -- n ) [: s" UNDERFLOW" POSTPONE-DEFINER:MAKE-BROKEN ;] catch ;
REJECTION:CAPTURE
BROKEN 70 T=
s" error[E-INPUT-UNDERFLOW]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" BROKEN 70 T=" T-PLACED
s" = word: drop" HAS  REJECTION:BUFFER-OFF
\ The built call is spelled like the file's token that ran its definer.
: CUBE ( -- n ) [: s" SHORT-CUBE" POSTPONE-DEFINER:MAKE-SHORT-CUBER ;] catch ;
REJECTION:CAPTURE
CUBE 70 T=
s" error[E-INPUT-UNDERFLOW]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" CUBE 70 T=" T-PLACED
s" = word: CUBE" HAS  REJECTION:BUFFER-OFF
s" : AFTER ( -- n ) 7 ;" evaluate-closed
AFTER 7 T=

: RV-REFUSED ( -- n ) [: s" RV-BAD" POSTPONE-DEFINER:MAKE-RV ;] catch ;
REJECTION:CAPTURE
RV-REFUSED 70 T=
s" error[E-NOT-EFFECT]" HAS  REJECTION:BUFFER$ s" postpone-define.f" 1 s" RV-REFUSED 70 T=" T-PLACED
s" = word: RV-BAD" HAS  REJECTION:BUFFER-OFF
s" : AFTER-RV ( -- n ) 8 ;" evaluate-closed
AFTER-RV 8 T=

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

T-REPORT
: MAIN ( -- ) POSTPONE-USER:ANSWERS ;
