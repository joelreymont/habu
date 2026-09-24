\ result-test.f - distinct ok/err payload type refusals. Run:
\   bin/hb --load lib/errors.f lib/string.f lib/test.f test/checker-assert.f \
\     lib/adt/result.f lib/adt/result-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f
require lib/adt/result.f

\ Negative type checks over a result with DISTINCT ok/err types
\ (result<n, ptr u8>). CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type
\ error), 1 uncheckable (undefined word). Asserting 0 proves the checker forces
\ the payload types to line up.
: RT-NEG ( -- )
   \ well-formed baselines: the right payload binds the right param -> accepted.
   s" RN0 ( n -- result<n,ptr u8> ) RESULT:OK"        CHECK-QUIET-CANDIDATE! -1 T=
   s" RN1 ( ptr u8 -- result<n,ptr u8> ) RESULT:ERR"  CHECK-QUIET-CANDIDATE! -1 T=
   \ swapped: OK given the ERR type (ptr u8, param b) -> rejected.
   s" RN2 ( ptr u8 -- result<n,ptr u8> ) RESULT:OK"   CHECK-QUIET-CANDIDATE! 0 T=
   \ swapped: ERR given the OK type (n, param a) -> rejected.
   s" RN3 ( n -- result<n,ptr u8> ) RESULT:ERR"       CHECK-QUIET-CANDIDATE! 0 T=
   \ reading the err payload as if it were the ok type: the err arm leaves
   \ ptr u8, the declared result is n, so the arms cannot unify -> rejected.
   s" RN4 ( result<n,ptr u8> -- n ) MATCH result ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T= ;

T-RESET
RT-NEG
T-REPORT
