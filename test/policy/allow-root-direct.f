require src/lib/vocabulary.f
require src/lib/string.f
using VOCABULARY

: RUN ( -- )
   s" test/policy/confined" >str SOURCE-ROOT!
   RESTRICT
   SB-RESET SOURCE-ROOT:ENGINE$ str> SB-APPEND
   s" /test/policy/confined/link.f" SB-APPEND
   SB$ >str SOURCE-ROOT:RUN-PROGRAM ;
RUN
;using
