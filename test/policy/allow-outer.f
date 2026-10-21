\ allow-outer.f - test/policy/allow.f with the loaded-bytes seam bound to the
\ interpret loop written in Habu (test/outer-loop-on.f) before the harness
\ seals: the sealed load is still read by the engine's loop.
require src/lib/vocabulary.f
require src/lib/ffi-abi.f
require src/lib/adt/option.f
require test/policy/dep.f
require test/policy/foreign.f
require test/outer-loop-on.f
using VOCABULARY

: RUN ( -- ) s" PDEP" ALLOW-PACKAGE RESTRICT 0 SCRIPT-ARGV$ included ;
RUN
;using
