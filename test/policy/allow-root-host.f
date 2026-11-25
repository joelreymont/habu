require src/lib/vocabulary.f
require src/habu/verify-source.f
require test/policy/dep.f

: PREFLIGHT ( str str -- ) {: text:str path:str :}
   [: [: text path path VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] CHECK:SCOPED ;]
   catch . ;
: LOAD ( str -- ) {: path:str :}
   [: path required ;] catch . ;
: CONFINED ( -- )
   s" : M ( -- ) 4 PDEP:SHOW ;" s" test/policy/confined/host.f" PREFLIGHT
   S\" require part.f\nPART:SHOW\n" s" test/policy/confined/scene.f" PREFLIGHT
   s" host.f" LOAD ;
: RUN ( -- )
   s" test/policy/confined" [: CONFINED ;] VOCABULARY:WITH-SOURCE-ROOT
   s" test/policy/outside.f" LOAD cr ;
RUN

\ Two scopes: the second's file calls a word of one the first loaded.
: SCOPES ( -- )
   s" test/policy/confined" [: s" part.f" LOAD ;] VOCABULARY:WITH-SOURCE-ROOT
   s" test/policy/confined" [: s" unrequired.f" LOAD ;] VOCABULARY:WITH-SOURCE-ROOT ;
: SAID-IN ( -- )
   s" : SAID ( -- n ) 5 ;" evaluate-closed  s" said.f" LOAD ;
SCOPES
s" test/policy/confined" ' SAID-IN VOCABULARY:WITH-SOURCE-ROOT
s" test/policy/confined" VOCABULARY:SOURCE-ROOT!
: LATE ( -- n ) 6 ;
: LATER ( -- )
   s" : M ( -- ) LATE PDEP:SHOW ;" s" test/policy/confined/late.f" PREFLIGHT
   s" : M ( -- ) LATE PDEP:SHOW ;" s" ./test/policy/confined/host.f" PREFLIGHT
   s" test/policy/confined/late.f" LOAD ;
LATER
: LAX ( -- ) s" ." FS:EXISTS? if 1 . then ;
