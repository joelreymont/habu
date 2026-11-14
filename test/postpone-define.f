\ postpone-define.f - a definer builds a definition with define-word,
\ body-append, literal and postpone, and each postponed word binds the word
\ the definer found when it was compiled, whatever the building package
\ defines: a global, the definer package's private word and its public one.
\ The same body evaluated as text binds the building package's words, and
\ body-append or literal with no definition open is refused. define-word takes
\ the effect a definer built, a parametric one included, and refuses a type
\ that is not an effect. A definition the checker refuses at the definer's
\ postpone ; is abandoned, and the next definition compiles. An effect holds
\ at most 23 type parameters, numbered per effect however many the file made
\ before. A STRUCTURE's AT answers its element in a package with its own `*`
\ and `record-at`. The file is also a build source: MAIN prints the four
\ answers.

require src/lib/test.f
require src/lib/adt/nullable.f

package POSTPONE-DEFINER
private
: BUMP ( n -- n ) 1 + ;
public
: DOUBLE ( n -- n ) 2 * ;

private
\ ( n -- n )
: N>N ( -- EFFECT:type ) EFFECT:NEW EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT ;

public
\ nullable on a parameter no other value holds.
: NULLABLE-T ( -- EFFECT:type )
   s" nullable" EFFECT:FAMILY ;

\ : NAME ( n -- n ) 3 * ;
: MAKE-TRIPLER ( ptr u8 n -- )
   N>N define-word
   s" 3" body-append
   postpone *
   postpone ; ;

\ : NAME ( n -- n ) BUMP DOUBLE ;
: MAKE-STEPPER ( ptr u8 n -- )
   N>N define-word
   postpone BUMP
   postpone DOUBLE
   postpone ; ;

\ : NAME ( -- n ) -6 7 * ;
: MAKE-NEG-ANSWER ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:OUT define-word
   -6 literal  7 literal
   postpone *
   postpone ; ;

\ : NAME ( nullable<a> -- nullable<a> nullable<a> ) dup ; one type value in
\ all three places.
: MAKE-DUP ( ptr u8 n -- )
   NULLABLE-T {: t :}
   EFFECT:NEW t EFFECT:IN  t EFFECT:OUT  t EFFECT:OUT  define-word
   postpone dup
   postpone ; ;

\ : NAME ( nullable<a> nullable<b> ... -- ) drop drop ... ; K inputs.
: MAKE-WIDE ( ptr u8 n n -- ) {: k:n :}
   EFFECT:NEW  k 0 ?do NULLABLE-T EFFECT:IN loop  define-word
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

\ : NAME ( -- n ) drop ;, an underflow the checker refuses.
: MAKE-BROKEN ( ptr u8 n -- )
   EFFECT:NEW EFFECT:N EFFECT:OUT define-word
   postpone drop
   postpone ; ;

\ : NAME ( -- read-view<a,b,c> ) 1 throw ;, a view no input supplies.
: MAKE-RV ( ptr u8 n -- )
   EFFECT:NEW s" read-view" EFFECT:FAMILY EFFECT:OUT define-word
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
s" NEG-ANSWER" POSTPONE-DEFINER:MAKE-NEG-ANSWER
s" : TRIPLE-TEXT ( n -- n ) 3 * ;" evaluate-closed

7 TRIPLE 21 T=
7 STEP 16 T=
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

: NOT-EFFECT ( -- n ) [: s" NOT-BUILT" EFFECT:N define-word ;] catch ;
NOT-EFFECT 76 T=

\ 23 type parameters fit an effect, a 24th is E-NOT-EFFECT.
: WIDE ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   [: a u k POSTPONE-DEFINER:MAKE-WIDE ;] catch ;
s" WIDE-23" 23 WIDE 0 T=
s" WIDE-24" 24 WIDE 76 T=

\ A spelling that names no family is E-UNKNOWN-SIGNATURE-TYPE.
: NO-FAMILY ( -- n ) [: s" no-such-family" EFFECT:FAMILY drop ;] catch ;
NO-FAMILY 70 T=

\ EFFECT:IN given a type that is no effect is E-NOT-EFFECT.
: IN-NOT-EFFECT ( -- n ) [: EFFECT:N EFFECT:N EFFECT:IN drop ;] catch ;
IN-NOT-EFFECT 70 T=

: APPEND-NONE ( -- n ) [: s" 1" body-append ;] catch ;
APPEND-NONE 0 T<>
: LITERAL-NONE ( -- n ) [: 1 literal ;] catch ;
LITERAL-NONE 70 T=

: BROKEN ( -- n ) [: s" UNDERFLOW" POSTPONE-DEFINER:MAKE-BROKEN ;] catch ;
BROKEN 70 T=
s" : AFTER ( -- n ) 7 ;" evaluate-closed
AFTER 7 T=

: RV-REFUSED ( -- n ) [: s" RV-BAD" POSTPONE-DEFINER:MAKE-RV ;] catch ;
RV-REFUSED 76 T=
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
