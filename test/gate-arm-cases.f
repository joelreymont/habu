\ These tests need the ARM host or its source recovery.

\ shadow-kinds.f reads a native arm64 shadow beside the host's tier-0 routines.
SUITE compiler-shadow
   test/compiler/shadow-kinds.f
;SUITE

\ arm64-place.f checks the product kernel's entry map, compares a placed arm64
\ shadow emission with the host's tier-0 routine and places every emission, then
\ only what a root reaches, and refuses a reached word a stripped program cannot run.
SUITE arm64-place
   test/compiler/arm64-place.f
;SUITE

\ arm64-link-data.f links a stripped build's closure, DATA image, header cells
\ and DATA list; it refuses a DATA pointer outside the space and a header store.
SUITE arm64-link-data
   test/compiler/arm64-link-data.f
;SUITE

\ ARM BRK breakpoint and resume fixtures select tier 0. The x64 kernel
\ explicitly refuses that selection; its crash/stack guards have native rows.
SUITE engine-stack-debugger
   test/engine-stack-debugger.f
;SUITE

SUITE debugger-resume
   test/debugger-resume.f
;SUITE

\ The harness selects tier 0 before its first definition: tier 1 refuses
\ CLEAR-MEAS's row drain (test/prop-test.f).
WHITEBOX-SUITE prop
   test/prop-test.f
;SUITE

\ arm64 reads files through the engine's loop: outer-loop-on.f gives cell-body
\ to the Habu loop, whose `create` (definers.f DEF-ADDR-TAIL) x86-64 runs.
SUITE cell-body-habu-loop
   test/outer-loop-on.f
   ENTRIES
   test/cell-body-e2e.f
;SUITE
