\ allow-sealed-admit.f - admission after the seal is rejected.
require src/lib/vocabulary.f
require test/policy/dep.f
require test/policy/foreign.f

: RUN ( -- ) s" PDEP" VOCABULARY:ALLOW-PACKAGE VOCABULARY:RESTRICT s" PFOREIGN" VOCABULARY:ALLOW-PACKAGE ;
RUN
