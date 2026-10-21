require src/lib/vocabulary.f
using VOCABULARY

: SEAL-IN ( -- )
   s" test/policy/confined" [: RESTRICT ;] WITH-SOURCE-ROOT ;
: CHANGE ( -- ) s" test/policy" SOURCE-ROOT! ;
: CHANGE-SCOPE ( -- ) s" test/policy" [: ;] WITH-SOURCE-ROOT ;
: OUTSIDE ( -- ) s" test/policy/outside.f" SOURCE-ROOT:RESOLVE drop 2drop ;
: RUN ( -- )
   [: SEAL-IN ;] catch .
   [: CHANGE ;] catch .
   [: CHANGE-SCOPE ;] catch .
   [: OUTSIDE ;] catch . cr ;
RUN
;using
