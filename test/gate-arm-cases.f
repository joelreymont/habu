\ These rows inspect the ARM host, source recovery, or its tier-0 JIT.
\ The Linux x86-64 engine has only tier 1; its native compiler, image,
\ and runtime rows stay in the main registry.

\ X64 is a second shadow target beside the ARM host. On an x64 host the same
\ target's resident map has different records (29 instead of this fixture's 6).
SUITE compiler-shadow
   test/compiler/shadow.f
;SUITE

\ Both fixtures open an ARM AOT source window and select tier 0 before asking
\ how an x64 shadow is carried or linked from that ARM window.
SUITE x86-64-link-records
   test/x86-64-link-records.f
;SUITE

SUITE aot-shadow-capture
   test/aot-shadow-capture.f
;SUITE

\ Direct JIT return and loop-cell assertions address ARM stack transfer
\ machinery; on x64 they return normally instead of the expected cell bounds.
SUITE engine-stack-jit
   test/engine-stack-jit.f
;SUITE

\ ARM BRK breakpoint and resume fixtures select tier 0. The x64 kernel
\ explicitly refuses that selection; its crash/stack guards have native rows.
SUITE engine-stack-debugger
   test/engine-stack-debugger.f
;SUITE

SUITE debugger-resume
   test/debugger-resume.f
;SUITE

\ This reads 4-byte AArch64 BL encodings and the JIT address-map bitmap.
SUITE addrmap-call
   test/addrmap-call.f
;SUITE

\ Its unchecked candidate and false-reject oracle require tier 0: the
\ harness selects tier 0 before defining or running any generated case.
WHITEBOX-SUITE prop
   test/prop-test.f
;SUITE

\ Both cases sabotage ARM recovery source and install its source-built engine.
\ Intel recovery cross-builds from ARM; native generations use native-build.f.
SUITE build-fixpoint-sandbox
   tools/build-fixpoint-sandbox-test.f
;SUITE

\ Version 1 NBR objects carry AArch64 instructions and relocations. These
\ rows execute an imported unit and require the keyed ARM unit export.
SUITE native-unit
   test/native-unit-e2e.f
;SUITE

SUITE native-unit-stale
   test/native-unit-stale.f
;SUITE
