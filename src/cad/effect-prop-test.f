\ effect-prop-test.f - metamorphic property suite for the CAD effect-row algebra
\ (dot habu-define-finite-cad-0bdf52ad). Run:
\   bin/hb --load src/cad/effect-prop-test.f
\
\ Over many pseudo-random effect-binding sets it asserts the identity invariants
\ that must hold for any canonical, allocation-order-independent row: insertion
\ order is not identity, UNION is commutative and idempotent, REMAP is
\ deterministic, and the canonical codec round-trips - all decided by canonical
\ CONTENT (EQUAL?), never by handle number or build order. Bindings are made
\ distinct by index so a set never collapses under the direct-duplicate rule. A
\ fixed seed keeps runs reproducible. Direct-loaded (lib/nominal precedent);
\ reopens package CAD-EFFECT for the array-driven builders.

require src/cad/effect.f
require lib/test.f

create EP-AC 512 cells allot     \ per-binding atom code (1..9)
create EP-KC 512 cells allot     \ per-binding kind code (0..3)
create EP-IX 512 cells allot     \ per-binding slot index (= i, so all distinct)
variable EP-SEED

package CAD-EFFECT
private

: EP-RAND ( -- n )
   EP-SEED @ 6364136223846793005 * 1442695040888963407 + dup EP-SEED !
   33 rshift $7FFFFFFF and ;

: EP-ATOM ( n -- effect-atom )                 \ 1..9 -> effectful atom
   case
      1 of PARAM-READ endof        2 of STATE-WRITE endof
      3 of RANDOM endof            4 of HOST-IO endof
      5 of DEVICE-LAUNCH endof     6 of ATOMIC endof
      7 of COLLECTIVE endof        8 of ALLOCATION endof
      9 of PUBLICATION endof
      PUBLICATION swap
   endcase ;
: EP-KIND ( n -- slot-kind )
   case
      0 of OPERAND endof     1 of ATTRIBUTE endof
      2 of CAPABILITY endof  3 of CAPTURE endof
      OPERAND swap
   endcase ;

: EP-AC@ ( n -- n )   cells EP-AC + @ ;
: EP-KC@ ( n -- n )   cells EP-KC + @ ;
: EP-IX@ ( n -- n )   cells EP-IX + @ ;

: EP-GEN ( n -- )                              \ fill the arrays with m distinct bindings
   {: m:n :}
   m 0 do
      EP-RAND 9 mod 1 +  i cells EP-AC + !
      EP-RAND 4 mod      i cells EP-KC + !
      i                  i cells EP-IX + !
   loop ;

: EP-ADD ( nom-builder n -- nom-builder ) {: k:n :}   \ EMIT binding k
   k EP-AC@ EP-ATOM  k EP-KC@ EP-KIND  k EP-IX@  EMIT ;

: EP-FWD ( n -- effect-row ) {: m:n :}
   NEW m 0 do i EP-ADD loop FREEZE ;
: EP-REV ( n -- effect-row ) {: m:n :}
   NEW m 0 do m 1- i - EP-ADD loop FREEZE ;
: EP-HALF ( n n -- effect-row ) {: lo:n hi:n :}
   NEW hi lo do i EP-ADD loop FREEZE ;

create EP-BUF 65536 allot
: EP-CODEC? ( n -- bool ) {: m:n :}
   m EP-FWD {: r:effect-row :}
   r EP-BUF 65536 ENCODE  EP-BUF swap DECODE  r EQUAL? ;

: EP-ONE ( n -- ) {: m:n :}                    \ one iteration over an m-binding set
   RESET
   m EP-GEN
   m EP-FWD  m EP-REV  EQUAL? TTRUE                  \ insertion order is not identity
   m EP-FWD  m EP-FWD  UNION  m EP-FWD  EQUAL? TTRUE \ UNION idempotent
   m 2 / {: h:n :}
   0 h EP-HALF  h m EP-HALF  UNION
   h m EP-HALF  0 h EP-HALF  UNION  EQUAL? TTRUE     \ UNION commutative
   m EP-FWD 5 REMAP  m EP-FWD 5 REMAP  EQUAL? TTRUE  \ REMAP deterministic
   m EP-CODEC? TTRUE ;                               \ canonical codec round-trip

: EP-ALL ( -- )
   T-RESET
   1 EP-SEED !
   64 0 do
      8 EP-RAND 120 mod +                            \ m in [8,128)
      EP-ONE
   loop
   T-REPORT ;

EP-ALL
;package
