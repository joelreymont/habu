\ checker-assert.f - shared quiet checker verdict helper for tests.

TRUSTED: CHECK-QUIET-CANDIDATE! ( ptr u8 n -- n )
   1 DIAG-QUIET +!
   CHECK-CANDIDATE!
   -1 DIAG-QUIET +! ;
