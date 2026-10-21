\ allow.f - the policy harness: admit PDEP, seal, load the design named after `--`.
\ src/lib/adt/option.f gives a design a wide type, `option<n>`, for its locals.
\ Run: bin/hb --load test/policy/allow.f -- test/policy/<case>.f
require src/lib/vocabulary.f
require src/lib/ffi-abi.f
require src/lib/adt/option.f
require test/policy/dep.f
require test/policy/foreign.f
using VOCABULARY

: RUN ( -- ) s" PDEP" ALLOW-PACKAGE RESTRICT 0 SCRIPT-ARGV$ included ;
RUN
;using
