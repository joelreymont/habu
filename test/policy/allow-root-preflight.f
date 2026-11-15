require src/lib/vocabulary.f
require src/habu/verify-source.f

: CHECK-OK ( -- )
   [: [: S\" require sub/keep.f\n" s" test/policy/confined/scene.f"
         s" test/policy/confined/scene.f"
         VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] CHECK:SCOPED ;] catch
   . ;
: CHECK-IN ( -- )
   [: [: S\" require ../outside.f\n" s" test/policy/confined/scene.f"
         s" test/policy/confined/scene.f"
         VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] CHECK:SCOPED ;] catch
   . ;
: RUN ( -- )
   s" test/policy/confined" [: CHECK-OK CHECK-IN ;] VOCABULARY:WITH-SOURCE-ROOT
   s" test/policy/outside.f" SOURCE-ROOT:RESOLVE drop 2drop
   0 . cr ;
RUN
