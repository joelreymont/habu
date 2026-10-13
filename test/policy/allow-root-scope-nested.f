require lib/policy.f

: VALUE-PATH ( -- ) s" test/policy/confined/value.f" SOURCE-ROOT:RESOLVE drop 2drop ;
: INNER ( -- )
   s" test/policy/confined/sub" [: VALUE-PATH ;] POLICY:WITH-ROOT ;
: OUTER ( -- )
   [: INNER ;] catch .
   VALUE-PATH 0 . ;
: RUN ( -- )
   s" test/policy/confined" [: OUTER ;] POLICY:WITH-ROOT
   s" test/policy/outside.f" SOURCE-ROOT:RESOLVE drop 2drop
   0 . cr ;
RUN
