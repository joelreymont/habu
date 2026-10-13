require src/lib/policy.f
require src/habu/verify-source.f

: CHECK-OK ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   [: S\" require sub/keep.f\n" s" test/policy/confined/scene.f"
      VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE
   . ;
: CHECK-IN ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   [: S\" require ../outside.f\n" s" test/policy/confined/scene.f"
      VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE
   . ;
: RUN ( -- )
   s" test/policy/confined" [: CHECK-OK CHECK-IN ;] POLICY:WITH-ROOT
   s" test/policy/outside.f" SOURCE-ROOT:RESOLVE drop 2drop
   0 . cr ;
RUN
