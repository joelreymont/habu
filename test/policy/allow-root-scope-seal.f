require lib/policy.f

: SEAL-IN ( -- )
   s" test/policy/confined" [: POLICY:SEAL ;] POLICY:WITH-ROOT ;
: CHANGE ( -- ) s" test/policy" POLICY:ROOT ;
: CHANGE-SCOPE ( -- ) s" test/policy" [: ;] POLICY:WITH-ROOT ;
: OUTSIDE ( -- ) s" test/policy/outside.f" SOURCE-ROOT:RESOLVE drop 2drop ;
: RUN ( -- )
   [: SEAL-IN ;] catch .
   [: CHANGE ;] catch .
   [: CHANGE-SCOPE ;] catch .
   [: OUTSIDE ;] catch . cr ;
RUN
