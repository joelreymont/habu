require src/lib/vocabulary.f

: BOOM ( -- )
   s" test/policy/confined" [: -30001 throw ;] VOCABULARY:WITH-SOURCE-ROOT ;
: RUN ( -- )
   [: BOOM ;] catch .
   s" test/policy/outside.f" SOURCE-ROOT:RESOLVE drop 2drop
   0 . cr ;
RUN
