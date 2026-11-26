require src/lib/vocabulary.f
require test/policy/dep.f

: RUN ( -- )
   s" test/policy/confined" VOCABULARY:SOURCE-ROOT!
   s" PDEP" VOCABULARY:ALLOW-PACKAGE
   VOCABULARY:RESTRICT
   0 SCRIPT-ARGV$ script-required ;
RUN
