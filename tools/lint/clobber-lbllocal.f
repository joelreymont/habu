\ clobber-lbllocal.f - fixture read as text by clobber-lint-test.f (never loaded).

\ A local spelled LBL, lives only in its arm, where it is a value, never a
\ label; the branch carries x9, written and clobbered, to m, where it is read.
\ Real: 1.
variable LLL-CALLEE
variable LLL-OUT
: EMIT-LL-CALLEE ( -- )
   LLL-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-LL ( bool -- ) {: f:bool :}
   LBL {: m:label :}
   9 0 MOVZ,
   LLL-CALLEE LABEL@ BL,
   10 m CBZ,
   LLL-OUT LABEL@ B,
   f if 5 {: LBL,:n :} LBL, drop then
   m LBL,
   4 9 0 LDRB, ;
