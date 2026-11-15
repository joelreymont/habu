\ ffi-callback-require.f - src/lib/ffi-callback-test.f CASE-REQUIRED requires
\ this file between a CALLBACK: declaration's clauses; its word is the body.
: CBT-REQUIRED-ORDER ( ptr u8 ptr u8 -- n )
   {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ - ;
