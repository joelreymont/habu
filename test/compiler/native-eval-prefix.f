\ A native definition stays open while an immediate evaluates a source fragment.
\ The fragment's own refusal must reach its catch before the outer `;`.
require lib/test.f
1 set-tier

package NATIVE-EVAL-PREFIX-TEST
public

variable BAD-CODE
variable PART-CODE
variable TICK-CODE
variable IS-CODE

: BAD ( -- )
   [: s" NATIVE-EVAL-NO-WORD" evaluate-closed ;] catch BAD-CODE ! ; immediate
s" NATIVE-EVAL-PREFIX-TEST:BAD" 0 parse-imm

: ADD-TWO ( -- ) s" 2 +" evaluate-closed ; immediate
s" NATIVE-EVAL-PREFIX-TEST:ADD-TWO" 0 parse-imm

: USE-LOCAL ( -- ) s" x 1 +" evaluate-closed ; immediate
s" NATIVE-EVAL-PREFIX-TEST:USE-LOCAL" 0 parse-imm

: USE-CHAR ( -- ) s" [char] Z" evaluate-closed ; immediate
s" NATIVE-EVAL-PREFIX-TEST:USE-CHAR" 0 parse-imm

: USE-QUOT ( -- ) s" [: 5 ;]" evaluate-closed ; immediate
s" NATIVE-EVAL-PREFIX-TEST:USE-QUOT" 0 parse-imm

: USE-BRANCH ( -- ) s" if 1 else 2 then" evaluate-closed ; immediate
s" NATIVE-EVAL-PREFIX-TEST:USE-BRANCH" 0 parse-imm

: PART ( -- )
   [: s" 1 + NATIVE-EVAL-NO-WORD" evaluate-closed ;] catch PART-CODE ! ; immediate
s" NATIVE-EVAL-PREFIX-TEST:PART" 0 parse-imm

: BAD-TICK ( -- )
   [: s" ['] NATIVE-EVAL-NO-TARGET" evaluate-closed ;] catch TICK-CODE ! ; immediate
s" NATIVE-EVAL-PREFIX-TEST:BAD-TICK" 0 parse-imm

: BAD-IS ( -- )
   [: s" is NATIVE-EVAL-NO-TARGET" evaluate-closed ;] catch IS-CODE ! ; immediate
s" NATIVE-EVAL-PREFIX-TEST:BAD-IS" 0 parse-imm

: BAD-CASE ( -- )
   0 BAD-CODE !
   [: s" : NEP-BAD-HOST ( -- n ) NATIVE-EVAL-PREFIX-TEST:BAD 5 ;" evaluate-closed ;]
   catch {: code:n :}
   code 0 T=
   BAD-CODE @ 70 T=
   code 0= if s" NEP-BAD-HOST 5 T=" evaluate-closed then ;

: GOOD-CASE ( -- )
   [: s" : NEP-ADD ( n -- n ) NATIVE-EVAL-PREFIX-TEST:ADD-TWO ;" evaluate-closed ;]
   catch 0 T=
   s" 40 NEP-ADD 42 T=" evaluate-closed
   [: s" : NEP-LOCAL ( n -- n ) {: x:n :} NATIVE-EVAL-PREFIX-TEST:USE-LOCAL ;" evaluate-closed ;]
   catch 0 T=
   s" 41 NEP-LOCAL 42 T=" evaluate-closed
   [: s" : NEP-CHAR ( -- n ) NATIVE-EVAL-PREFIX-TEST:USE-CHAR ;" evaluate-closed ;]
   catch 0 T=
   s" NEP-CHAR 90 T=" evaluate-closed
   [: s" : NEP-QUOT ( -- n ) NATIVE-EVAL-PREFIX-TEST:USE-QUOT execute ;" evaluate-closed ;]
   catch 0 T=
   s" NEP-QUOT 5 T=" evaluate-closed
   [: s" : NEP-BRANCH ( bool -- n ) NATIVE-EVAL-PREFIX-TEST:USE-BRANCH ;" evaluate-closed ;]
   catch 0 T=
   s" true NEP-BRANCH 1 T= false NEP-BRANCH 2 T=" evaluate-closed ;

: PART-CASE ( -- )
   0 PART-CODE !
   [: s" : NEP-PART ( n -- n ) NATIVE-EVAL-PREFIX-TEST:PART ;" evaluate-closed ;]
   catch {: code:n :}
   code 0 T=
   PART-CODE @ 70 T=
   code 0= if s" 41 NEP-PART 42 T=" evaluate-closed then ;

: OPERAND-CASE ( -- )
   0 TICK-CODE !
   [: s" : NEP-TICK-HOST ( -- n ) NATIVE-EVAL-PREFIX-TEST:BAD-TICK 5 ;" evaluate-closed ;]
   catch {: tick-code:n :}
   tick-code 0 T=
   TICK-CODE @ 70 T=
   tick-code 0= if s" NEP-TICK-HOST 5 T=" evaluate-closed then
   0 IS-CODE !
   [: s" : NEP-IS-HOST ( -- n ) NATIVE-EVAL-PREFIX-TEST:BAD-IS 5 ;" evaluate-closed ;]
   catch {: is-code:n :}
   is-code 0 T=
   IS-CODE @ 70 T=
   is-code 0= if s" NEP-IS-HOST 5 T=" evaluate-closed then ;

: RUN ( -- )
   T-RESET
   BAD-CASE GOOD-CASE PART-CASE OPERAND-CASE
   T-REPORT ;

;package

NATIVE-EVAL-PREFIX-TEST:RUN
