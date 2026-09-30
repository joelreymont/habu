\ allow-outer.f - test/policy/allow.f with the loaded-bytes seam bound to the
\ interpret loop written in Habu (test/outer-loop-on.f) before the harness
\ seals: the sealed load is still read by the engine's loop.
require lib/policy.f
require lib/ffi-abi.f
require lib/adt/option.f
require test/policy/dep.f
require test/policy/foreign.f
require test/outer-loop-on.f

: RUN ( -- ) s" PDEP" POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ included ;
RUN
