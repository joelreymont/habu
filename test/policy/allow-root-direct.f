require src/lib/policy.f
require src/lib/string.f

: RUN ( -- )
   s" test/policy/confined" POLICY:ROOT
   POLICY:SEAL
   SB-RESET SOURCE-ROOT:ENGINE$ SB-APPEND
   s" /test/policy/confined/link.f" SB-APPEND
   SB$ SOURCE-ROOT:LOAD ;
RUN
