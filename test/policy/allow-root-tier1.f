require src/lib/vocabulary.f
require test/policy/dep.f
using VOCABULARY
1 set-tier

: RUN ( -- )
   s" test/policy/confined" SOURCE-ROOT!
   s" PDEP" ALLOW-PACKAGE
   RESTRICT
   0 SCRIPT-ARGV$ script-required ;
RUN
;using
