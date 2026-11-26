\ allow-seal-twice.f - sealing a sealed process is rejected.
require src/lib/vocabulary.f
require test/policy/dep.f

: RUN ( -- ) s" PDEP" VOCABULARY:ALLOW-PACKAGE VOCABULARY:RESTRICT VOCABULARY:RESTRICT ;
RUN
