require src/lib/vocabulary.f

: BOOM ( -- )
   s" test/policy/confined" >str [: -30001 throw ;] VOCABULARY:WITH-SOURCE-ROOT ;
: RUN ( -- )
   [: BOOM ;] catch .
   s" test/policy/outside.f" >str SOURCE-ROOT:RESOLVE
   drop str> 2drop
   0 . cr ;
RUN
