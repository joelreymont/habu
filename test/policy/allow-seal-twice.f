\ allow-seal-twice.f - sealing a sealed process is refused.
require src/lib/vocabulary.f
require test/policy/dep.f
using VOCABULARY

: RUN ( -- ) s" PDEP" ALLOW-PACKAGE RESTRICT RESTRICT ;
RUN
;using
