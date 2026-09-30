\ allow-catch.f - a refusal inside the harness's `included` frame is a throw
\ the harness can catch; it prints the code it caught.
require lib/policy.f
require test/policy/dep.f

: LOAD ( -- ) 0 SCRIPT-ARGV$ included ;
: RUN ( -- ) s" PDEP" POLICY:ALLOW POLICY:SEAL [: LOAD ;] catch . ;
RUN
