require src/lib/vocabulary.f

: SEAL-IN ( -- )
   s" test/policy/confined" [: VOCABULARY:RESTRICT ;] VOCABULARY:WITH-SOURCE-ROOT ;
: CHANGE ( -- ) s" test/policy" VOCABULARY:SOURCE-ROOT! ;
: CHANGE-SCOPE ( -- ) s" test/policy" [: ;] VOCABULARY:WITH-SOURCE-ROOT ;
: OUTSIDE ( -- ) s" test/policy/outside.f" SOURCE-ROOT:RESOLVE drop str> 2drop ;
: RUN ( -- )
   [: SEAL-IN ;] catch .
   [: CHANGE ;] catch .
   [: CHANGE-SCOPE ;] catch .
   [: OUTSIDE ;] catch . cr ;
RUN
