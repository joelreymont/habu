\ memory-test.f - focused tests for OS-backed memory buffers.

64 constant MEMT-BUFS
65 constant MEMT-MARK-A
90 constant MEMT-MARK-Z

variable MEMT-HERE

: MEMT-TOTAL ( -- n )
   MEMT-BUFS MEM-64K * ;

: MEMT-ZERO-BYTES ( -- )
   0 MEM-ALLOC-BYTES 2drop ;

: MEMT-NEG-BYTES ( -- )
   -1 MEM-ALLOC-BYTES 2drop ;

: MEMT-ZERO-64K ( -- )
   0 MEM-ALLOC-64K-BUFFERS 2drop ;

: MEMT-TOO-MANY-64K ( -- )
   MEM-MAX-64K-BUFFERS 1 + MEM-64K-BYTES drop ;

: MEMT-END ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   a u 1 - + ;

: MEMT-TOUCH-ENDS ( ptr u8 n -- ) {: a:ptr u :}
   MEMT-MARK-A a c!
   MEMT-MARK-Z a u MEMT-END c!
   a c@ MEMT-MARK-A T=
   a u MEMT-END c@ MEMT-MARK-Z T= ;

: MEMT-TOUCH-64K-SLOTS ( ptr u8 n -- ) {: a:ptr u :}
   u MEMT-TOTAL T=
   MEMT-BUFS 0 ?do
      MEMT-MARK-A i + a i MEM-64K * + c!
      a i MEM-64K * + c@ MEMT-MARK-A i + T=
   loop ;

: MEMT-SINGLE-64K ( -- )
   MEM-ALLOC-64K
   dup MEM-64K T=
   MEMT-TOUCH-ENDS ;

: MEMT-MANY-64K ( -- )
   MEMT-BUFS MEM-ALLOC-64K-BUFFERS
   MEMT-TOUCH-64K-SLOTS ;

: MEMT-DATA-UNCHANGED ( -- )
   here data-base - MEMT-HERE !
   MEM-ALLOC-64K 2drop
   here data-base - MEMT-HERE @ T= ;

T-RESET
MEM-64K $10000 T=
1 MEM-64K-BYTES MEM-64K T=
MEMT-SINGLE-64K
MEMT-MANY-64K
MEMT-DATA-UNCHANGED
' MEMT-ZERO-BYTES E-MEM-SIZE TTHROWS
' MEMT-NEG-BYTES E-MEM-SIZE TTHROWS
' MEMT-ZERO-64K E-MEM-SIZE TTHROWS
' MEMT-TOO-MANY-64K E-MEM-SIZE TTHROWS
T-REPORT
s" memory-test: ok" type cr
