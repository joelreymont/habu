\ A hook-less prefix definition's declaration is its row in the window's checker.
\ The window compiles its core prefix with the hook cell empty, so no scan judges
\ SCHEMA-REG:SCHEMA-CON (src/core/type-schema.f) or the checker's own CON-OF
\ (src/core/checker.f); each declaration is its row, recorded without authority
\ and copied to the window's checker with the rest. That checker is unsealed, so
\ a body naming either word certifies, and a caller a row does not fit is
\ refused by the row. Without the rows the definitions of W and W2 below are
\ E-UNDEFINED and the window answers 70.
package OWNER-DECLARED-ROW

$1000 constant IO-CAP
create DIAGS IO-CAP allot

: EQ! ( n n -- ) <> if 79 throw then ;
: YES! ( bool -- ) 0= if 79 throw then ;

\ Whether the bytes at A start with B. lib/ is absent in the window, and
\ src/core/util.f CORE-STR= declares no effect, so it has no row to name.
: STARTS? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   v u > if 1 0= exit then
   v 0 ?do
      a i + c@  b i + c@  <> if unloop 1 0= exit then
   loop
   0 0= ;

: HOLDS? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   u 1 + 0 ?do
      a i +  u i -  b v STARTS? if unloop 0 0= exit then
   loop
   1 0= ;

: W ( n -- n ) SCHEMA-REG:SCHEMA-CON ;
: W2 ( ptr u8 n -- n ) CON-OF ;

\ The candidate source certifies nothing: the row it calls refuses it.
: MISMATCH! ( ptr u8 n -- )
   {: src:ptr su:n :}
   DIAGS IO-CAP DIAG-BUFFER!  0 0= DIAG-JSON!
   src su CHECK-CANDIDATE! {: rc:n :}
   DIAG-BUFFER$ {: d:ptr du:n :}
   1 0= DIAG-JSON!  DIAG-BUFFER-OFF
   rc 0 EQ!
   d du s\" \"code\":\"E-MISMATCH\"" HOLDS? YES! ;

: RUN ( -- )
   s" SCHEMA-REG:SCHEMA-CON" EFFECT-QUERY YES!
   s" CON-OF" EFFECT-QUERY YES!
   s" WRONG ( n -- bool ) SCHEMA-REG:SCHEMA-CON" MISMATCH!
   s" WRONG2 ( ptr u8 n -- bool ) CON-OF" MISMATCH! ;

RUN
;package
