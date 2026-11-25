\ A subject not saved yet, under a root that is not in the engine's tree.
require src/lib/vocabulary.f
require src/habu/verify-source.f

: LATE ( -- n ) 6 ;
: PREFLIGHT ( str str -- ) {: text:str path:str :}
   [: [: text path path VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] CHECK:SCOPED ;]
   catch . ;
s" conf" VOCABULARY:SOURCE-ROOT!
: RUN ( -- ) s" : M ( -- ) LATE drop ;" s" conf/new.f" PREFLIGHT ;
RUN
