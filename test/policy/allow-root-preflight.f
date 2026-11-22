require src/lib/vocabulary.f
require src/habu/verify-source.f

: CHECK-OK ( -- )
   [: [: S\" require sub/keep.f\n" >str
         s" test/policy/confined/scene.f" >str
         s" test/policy/confined/scene.f" >str
         VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] CHECK:SCOPED ;] catch
   . ;
: CHECK-IN ( -- )
   [: [: S\" require ../outside.f\n" >str
         s" test/policy/confined/scene.f" >str
         s" test/policy/confined/scene.f" >str
         VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] CHECK:SCOPED ;] catch
   . ;
: RUN ( -- )
   s" test/policy/confined" >str [: CHECK-OK CHECK-IN ;] VOCABULARY:WITH-SOURCE-ROOT
   s" test/policy/outside.f" >str SOURCE-ROOT:RESOLVE drop str> 2drop
   0 . cr ;
RUN
