\ ad-dag-test.f - validation tests for PTX reverse-mode DAG builder.

require lib/ptx/test-prelude.f

T-RESET

AD-MAXN 1+ constant ADT-OPS-CAP
create ADT-OPS ADT-OPS-CAP cells allot

: ADT-OP! ( n n -- ) {: op:n idx:n :}
   op ADT-OPS idx cells + ! ;

: ADT-SOFTMAX-OPS ( -- )
   OP-DUP  0 ADT-OP!
   OP-BMAX 1 ADT-OP!
   OP-BSUB 2 ADT-OP!
   OP-EXP  3 ADT-OP!
   OP-DUP  4 ADT-OP!
   OP-BSUM 5 ADT-OP!
   OP-BDIV 6 ADT-OP! ;

: ADT-SOFTMAX ( -- )
   ADT-SOFTMAX-OPS
   ADT-OPS 7 AD-BUILD ;

: ADT-FILL-EXP ( -- )
   AD-MAXN 0 ?do OP-EXP i ADT-OP! loop ;

: ADT-OVERFLOW ( -- )
   ADT-FILL-EXP
   ADT-OPS AD-MAXN AD-BUILD ;

: ADT-UNDERFLOW ( -- )
   OP-BSUB 0 ADT-OP!
   ADT-OPS 1 AD-BUILD ;

: ADT-EXTRA-OUT ( -- )
   OP-DUP 0 ADT-OP!
   ADT-OPS 1 AD-BUILD ;

: ADT-UNKNOWN ( -- )
   $63 0 ADT-OP!
   ADT-OPS 1 AD-BUILD ;

ADT-SOFTMAX
AD-N @ 6 T=
AD-OUT @ 5 T=
AD-VSP @ 0 T=

' ADT-OVERFLOW E-PTX-AD-OVERFLOW TTHROWS
' ADT-UNDERFLOW E-PTX-AD-UNDERFLOW TTHROWS
' ADT-EXTRA-OUT E-PTX-AD-OUTPUT TTHROWS
' ADT-UNKNOWN E-PTX-AD-UNKNOWN TTHROWS

T-REPORT
