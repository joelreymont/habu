\ allow-seal-twice.f - sealing a sealed process is refused.
require src/lib/policy.f
require test/policy/dep.f

: RUN ( -- ) s" PDEP" POLICY:ALLOW POLICY:SEAL POLICY:SEAL ;
RUN
