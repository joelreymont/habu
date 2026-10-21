require src/lib/vocabulary.f
require src/lib/string.f
using VOCABULARY

: RUN ( -- )
   s" test/policy/confined" SOURCE-ROOT!
   RESTRICT
   SB-RESET SOURCE-ROOT:ENGINE$ SB-APPEND
   s" /test/policy/confined/link.f" SB-APPEND
   SB$ SOURCE-ROOT:LOAD ;
RUN
;using
