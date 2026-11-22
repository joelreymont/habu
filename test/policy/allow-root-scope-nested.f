require src/lib/vocabulary.f

: VALUE-PATH ( -- ) s" test/policy/confined/value.f" >str SOURCE-ROOT:RESOLVE drop str> 2drop ;
: INNER ( -- )
   s" test/policy/confined/sub" >str [: VALUE-PATH ;] VOCABULARY:WITH-SOURCE-ROOT ;
: OUTER ( -- )
   [: INNER ;] catch .
   VALUE-PATH 0 . ;
: RUN ( -- )
   s" test/policy/confined" >str [: OUTER ;] VOCABULARY:WITH-SOURCE-ROOT
   s" test/policy/outside.f" >str SOURCE-ROOT:RESOLVE drop str> 2drop
   0 . cr ;
RUN
