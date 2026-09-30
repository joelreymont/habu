\ allow-sealed-admit.f - admission after the seal is refused.
require lib/policy.f
require test/policy/dep.f
require test/policy/foreign.f

: RUN ( -- ) s" PDEP" POLICY:ALLOW POLICY:SEAL s" PFOREIGN" POLICY:ALLOW ;
RUN
