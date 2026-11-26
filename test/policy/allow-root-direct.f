require src/lib/vocabulary.f
require src/lib/string.f

: RUN ( -- )
   s" test/policy/confined" VOCABULARY:SOURCE-ROOT!
   VOCABULARY:RESTRICT
   SB-RESET SOURCE-ROOT:ENGINE$ SB-APPEND
   s" /test/policy/confined/link.f" SB-APPEND
   SB$ SOURCE-ROOT:RUN-PROGRAM ;
RUN
