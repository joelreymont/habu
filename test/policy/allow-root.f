require src/lib/vocabulary.f
require test/policy/dep.f
using VOCABULARY

: RUN ( -- )
   s" test/policy/confined" SOURCE-ROOT!
   s" PDEP" ALLOW-PACKAGE
   RESTRICT
   0 SCRIPT-ARGV$ script-required ;
RUN
;using
