require lib/policy.f
require test/policy/dep.f

: RUN ( -- )
   s" test/policy/confined" POLICY:ROOT
   s" PDEP" POLICY:ALLOW
   POLICY:SEAL
   0 SCRIPT-ARGV$ script-required ;
RUN
