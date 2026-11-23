\ allow-catch.f - a refusal inside the harness's `included` frame is a throw
\ the harness can catch; it prints the code it caught.
require src/lib/vocabulary.f
require test/policy/dep.f
using VOCABULARY

: LOAD ( -- ) 0 SCRIPT-ARGV$ included ;
: RUN ( -- ) s" PDEP" ALLOW-PACKAGE RESTRICT [: LOAD ;] catch . ;
RUN
;using
