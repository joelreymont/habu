\ allow-catch.f - a rejection inside the harness's `included` frame is a throw
\ the harness can catch; it prints the code it caught.
require src/lib/vocabulary.f
require test/policy/dep.f

: LOAD ( -- ) 0 SCRIPT-ARGV$ included ;
: RUN ( -- ) s" PDEP" VOCABULARY:ALLOW-PACKAGE VOCABULARY:RESTRICT [: LOAD ;] catch . ;
RUN
