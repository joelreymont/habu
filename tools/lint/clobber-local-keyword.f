\ clobber-local-keyword.f - label locals spelled as a loop closer and as an
\ emitting host word, read as text by clobber-lint-test.f (never loaded).

: SCRATCH ( n -- ) {: r:n :}
   r 0 MOVZ, ;

variable LKW-CALLEE

: EMIT-KW-CALLEE ( -- )
   LKW-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;                \ clobbers x9

\ scratch is the label local throughout: x9 held across the BL, one finding.
: EMIT-KW ( -- )
   LBL LBL {: loop:label scratch:label :}
   9 0 MOVZ,
   loop LBL,
   LKW-CALLEE LABEL@ BL,
   scratch LBL,
   4 9 0 LDRB, ;
