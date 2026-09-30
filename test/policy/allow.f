\ allow.f - the policy harness: admit PDEP, seal, load the design named after `--`.
\ lib/adt/option.f gives a design a wide type, `option<n>`, for its locals.
\ Run: bin/hb --load test/policy/allow.f -- test/policy/<case>.f
require lib/policy.f
require lib/ffi-abi.f
require lib/adt/option.f
require test/policy/dep.f
require test/policy/foreign.f

: RUN ( -- ) s" PDEP" POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ included ;
RUN
