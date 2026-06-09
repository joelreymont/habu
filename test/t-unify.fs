\ t-unify.fs — unifiers + occurs. Words below have ( -- ) effect so
\ ' WORD catch yields a clean ( code ).  Assumes all repr + unify loaded.

\ concrete type unify
: OK-CC    TC-I64 MK-CON TC-I64 MK-CON UNIFY-TYPE ;
: BAD-CC   TC-I64 MK-CON TC-BOOL MK-CON UNIFY-TYPE ;
T{ ' OK-CC  catch -> 0 }T
T{ ' BAD-CC catch -> E-MISMATCH }T

\ type var binds to a concrete
: VB   TV-RESET 2 TV-ALLOC drop  0 MK-VAR TC-I64 MK-CON UNIFY-TYPE ;
T{ VB  0 TV@ -> TC-I64 MK-CON }T

\ occurs: a ~ ptr<a>
: OCC-T  TV-RESET ARENA-RESET 1 TV-ALLOC drop  0 MK-VAR  0 MK-VAR MK-PTR  UNIFY-TYPE ;
T{ ' OCC-T catch -> E-OCCURS }T

\ quot/quot unify recurses into effects (i64-top vs var-top binds the var)
: QQ  TV-RESET ARENA-RESET 1 TV-ALLOC drop
   0 MK-ROW TC-I64 MK-CON MK-PUSH 0 MK-ROW 1 MK-ROW 1 MK-ROW MK-EFFECT MK-QUOT
   0 MK-ROW 0 MK-VAR     MK-PUSH 0 MK-ROW 1 MK-ROW 1 MK-ROW MK-EFFECT MK-QUOT
   UNIFY-TYPE ;
T{ QQ  0 TV@ -> TC-I64 MK-CON }T

\ row var binds to a push;  then push/push unifies tops
: RB   RV-RESET ARENA-RESET 4 RV-ALLOC drop
   3 MK-ROW  0 MK-ROW TC-I64 MK-CON MK-PUSH  UNIFY-ROW ;
T{ RB  3 RV@ SPUSH? -> true }T

: RP   RV-RESET TV-RESET ARENA-RESET 1 RV-ALLOC drop 6 TV-ALLOC drop
   0 MK-ROW TC-I64 MK-CON MK-PUSH
   0 MK-ROW 5 MK-VAR     MK-PUSH  UNIFY-ROW ;
T{ RP  5 TV@ -> TC-I64 MK-CON }T

\ row occurs: R ~ (R, i64)  -> infinite, rejected
: ROCC  RV-RESET ARENA-RESET 3 RV-ALLOC drop
   2 MK-ROW  2 MK-ROW TC-I64 MK-CON MK-PUSH  UNIFY-ROW ;
T{ ' ROCC catch -> E-OCCURS }T

\ omega: row 0 occurs inside a quotation on the stack (no loop, returns true)
: OMEGA  RV-RESET ARENA-RESET 3 RV-ALLOC drop
   0 MK-ROW 0 MK-ROW 1 MK-ROW 1 MK-ROW MK-EFFECT MK-QUOT   ( q with din=R0 )
   2 MK-ROW swap MK-PUSH                                    ( R2 q )
   0 swap OCCURS-ROW ;
T{ OMEGA -> true }T

\ row arity mismatch surfaces as occurs/mismatch, not silent success
: RARITY  RV-RESET ARENA-RESET 1 RV-ALLOC drop
   0 MK-ROW                                  \ R0
   0 MK-ROW TC-I64 MK-CON MK-PUSH            \ (R0, i64)
   UNIFY-ROW ;                               \ R0 ~ (R0,i64) -> occurs
T{ ' RARITY catch -> E-OCCURS }T
