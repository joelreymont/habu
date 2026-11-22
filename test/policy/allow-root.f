require src/lib/vocabulary.f
require test/policy/dep.f
using VOCABULARY

: RUN ( -- )
   s" test/policy/confined" >str SOURCE-ROOT!
   s" PDEP" >str ALLOW-PACKAGE
   RESTRICT
   0 SCRIPT-ARGV$ str> script-required ;
RUN
;using
