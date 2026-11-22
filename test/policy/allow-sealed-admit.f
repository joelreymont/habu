\ allow-sealed-admit.f - admission after the seal is refused.
require src/lib/vocabulary.f
require test/policy/dep.f
require test/policy/foreign.f
using VOCABULARY

: RUN ( -- ) s" PDEP" >str ALLOW-PACKAGE RESTRICT s" PFOREIGN" >str ALLOW-PACKAGE ;
RUN
;using
