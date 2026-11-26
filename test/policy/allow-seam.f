\ allow-seam.f - test/policy/allow.f with the loaded-bytes seam bound to a reader
\ that runs nothing before the harness seals: the sealed load still runs.
require src/lib/vocabulary.f
require src/lib/ffi-abi.f
require src/lib/adt/option.f
require test/policy/dep.f
require test/policy/foreign.f

: UNREAD ( -- ) [: 2drop drop ;] SOURCE-ROOT:USE-INTERPRETER ;
UNREAD

: RUN ( -- ) s" PDEP" VOCABULARY:ALLOW-PACKAGE VOCABULARY:RESTRICT 0 SCRIPT-ARGV$ included ;
RUN
