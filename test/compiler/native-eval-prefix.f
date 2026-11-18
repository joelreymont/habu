split-str
\ A definition its definer opened stays open while text is evaluated into it:
\ the text's own refusal reaches its catch before the definer's `;`.
require src/lib/test.f

package NATIVE-EVAL-PREFIX-TEST
public

variable BAD-CODE
variable PART-CODE
variable TICK-CODE
variable IS-CODE

: N>N ( -- EFFECT:type ) EFFECT:EMPTY EFFECT:N EFFECT:IN EFFECT:N EFFECT:OUT ;
: >N ( -- EFFECT:type ) EFFECT:EMPTY EFFECT:N EFFECT:OUT ;

\ : NAME ( -- n ) 5 ; where a text that names no word is refused first.
: MAKE-BAD ( str -- )
   >N define-word
   [: s" NATIVE-EVAL-NO-WORD" evaluate-closed ;] catch BAD-CODE !
   s" 5" evaluate-closed postpone ; ;

\ : NAME ( -- n ) 5 ; where a tick of no word is refused first.
: MAKE-BAD-TICK ( str -- )
   >N define-word
   [: s" ['] NATIVE-EVAL-NO-TARGET" evaluate-closed ;] catch TICK-CODE !
   s" 5" evaluate-closed postpone ; ;

\ : NAME ( -- n ) 5 ; where an `is` of no word is refused first.
: MAKE-BAD-IS ( str -- )
   >N define-word
   [: s" is NATIVE-EVAL-NO-TARGET" evaluate-closed ;] catch IS-CODE !
   s" 5" evaluate-closed postpone ; ;

: BAD-CASE ( -- )
   0 BAD-CODE !
   [: s" NEP-BAD-HOST" MAKE-BAD ;] catch {: code:n :}
   code 0 T=
   BAD-CODE @ 70 T=
   code 0= if s" NEP-BAD-HOST 5 T=" evaluate-closed then ;

\ : NAME E TEXT ; with the definition opened by define-word.
: MAKE-TEXT ( str EFFECT:type str -- ) {: ta:^ tu:n :}
   define-word ta tu evaluate-closed postpone ; ;

\ : NAME ( n -- n ) 1 + ; where the text's refusal is caught.
: MAKE-PART ( str -- )
   N>N define-word
   [: s" 1 + NATIVE-EVAL-NO-WORD" evaluate-closed ;] catch PART-CODE !
   postpone ; ;

: GOOD-CASE ( -- )
   [: s" NEP-ADD" N>N s" 2 +" MAKE-TEXT ;] catch 0 T=
   s" 40 NEP-ADD 42 T=" evaluate-closed
   [: s" NEP-LOCAL" N>N s" {: x:n :} x 1 +" MAKE-TEXT ;] catch 0 T=
   s" 41 NEP-LOCAL 42 T=" evaluate-closed
   [: s" NEP-CHAR" >N s" [char] Z" MAKE-TEXT ;] catch 0 T=
   s" NEP-CHAR 90 T=" evaluate-closed
   [: s" NEP-QUOT" >N s" [: 5 ;] execute" MAKE-TEXT ;] catch 0 T=
   s" NEP-QUOT 5 T=" evaluate-closed
   [: s" NEP-BRANCH" EFFECT:EMPTY s" bool" EFFECT:NAMED EFFECT:IN EFFECT:N EFFECT:OUT
      s" if 1 else 2 then" MAKE-TEXT ;] catch 0 T=
   s" true NEP-BRANCH 1 T= false NEP-BRANCH 2 T=" evaluate-closed ;

: PART-CASE ( -- )
   0 PART-CODE !
   [: s" NEP-PART" MAKE-PART ;] catch {: code:n :}
   code 0 T=
   PART-CODE @ 70 T=
   code 0= if s" 41 NEP-PART 42 T=" evaluate-closed then ;

: OPERAND-CASE ( -- )
   0 TICK-CODE !
   [: s" NEP-TICK-HOST" MAKE-BAD-TICK ;] catch {: tick-code:n :}
   tick-code 0 T=
   TICK-CODE @ 70 T=
   tick-code 0= if s" NEP-TICK-HOST 5 T=" evaluate-closed then
   0 IS-CODE !
   [: s" NEP-IS-HOST" MAKE-BAD-IS ;] catch {: is-code:n :}
   is-code 0 T=
   IS-CODE @ 70 T=
   is-code 0= if s" NEP-IS-HOST 5 T=" evaluate-closed then ;

: RUN ( -- )
   T-RESET
   BAD-CASE GOOD-CASE PART-CASE OPERAND-CASE
   T-REPORT ;

;package

NATIVE-EVAL-PREFIX-TEST:RUN
