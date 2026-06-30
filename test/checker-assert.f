\ checker-assert.f - shared quiet checker verdict helper for tests.

TRUSTED: CHECK-QUIET-CANDIDATE! ( ptr u8 n -- n )
   DIAGXT @ >r
   0 DIAGXT !
   CHECK-CANDIDATE!
   r> DIAGXT ! ;
