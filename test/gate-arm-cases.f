\ These rows need the ARM host, its source recovery or the JIT.

\ X64 is a second shadow target beside the ARM host. On an x64 host the same
\ target's resident map has different records (29 instead of shadow.f's 6).
\ shadow-kinds.f reads a native arm64 shadow beside the host's tier-0 routines.
SUITE compiler-shadow
   test/compiler/shadow.f
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

\ The fixture opens an ARM AOT source window and selects tier 0 before asking
\ how an x64 shadow is linked from that ARM window.
SUITE x86-64-link-records
   test/x86-64-link-records.f
;SUITE

\ ARM BRK breakpoint and resume fixtures select tier 0. The x64 kernel
\ explicitly refuses that selection; its crash/stack guards have native rows.
SUITE engine-stack-debugger
   test/engine-stack-debugger.f
;SUITE

SUITE debugger-resume
   test/debugger-resume.f
;SUITE

SUITE compiler-native-wide-frame
   test/compiler/native-wide-frame.f
;SUITE

\ The harness selects tier 0 before its first definition: tier 1 refuses
\ CLEAR-MEAS's row drain (test/prop-test.f).
WHITEBOX-SUITE prop
   test/prop-test.f
;SUITE

\ The arm64 JIT copies straight-line primitive bodies.
SUITE compiler-tier0-inline-prims
   test/compiler/tier0-inline-prims.f
;SUITE

\ Tier 0 branches on a compare's flags, read as arm64 instructions.
SUITE compiler-tier0-compare-branch
   test/compiler/tier0-compare-branch.f
;SUITE

\ The arm64 JIT reconciles the virtual stack at a loop's back edge.
SUITE compiler-tier0-join
   test/compiler/tier0-join.f
;SUITE

\ Tier 0 string literal bodies are INTERN rows, not bytes in the code band.
SUITE compiler-tier0-string-literals
   test/compiler/tier0-string-literals.f
;SUITE

\ arm64 reads files through the engine's loop: outer-loop-on.f gives cell-body
\ to the Habu loop, whose `create` (definers.f DEF-ADDR-TAIL) x86-64 runs.
SUITE cell-body-habu-loop
   test/outer-loop-on.f
   ENTRIES
   test/cell-body-e2e.f
;SUITE
