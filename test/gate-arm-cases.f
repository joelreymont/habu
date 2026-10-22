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

\ ARM BRK breakpoint and resume fixtures select tier 0. The x64 kernel
\ explicitly refuses that selection; its crash/stack guards have native rows.
SUITE engine-stack-debugger
   test/engine-stack-debugger.f
;SUITE

SUITE debugger-resume
   test/debugger-resume.f
;SUITE

\ These read source definitions as fixed-width AArch64 instructions and
\ inspect SP slots. Their runtime subjects are checked in those suites.
SUITE compiler-native-identity-spill
   test/compiler/native-identity-spill.f
;SUITE

SUITE compiler-native-wide-frame
   test/compiler/native-wide-frame.f
;SUITE

\ Live ARM windows and instruction-chain compaction need an ARM host. Inert
\ metadata storage, transfer and refusal cases stay shared.
SUITE aot-chain-capture
   test/aot-chain-capture-suite.f
;SUITE

\ Mutations target the ARM boot reader's packed ULEB record/site tables.
\ The ordinary seed-batch build and execution row stays shared.
SUITE aot-seed-metadata
   test/aot-seed-metadata.f
;SUITE

\ Stages the ARM seed emitter's signature payload (habu2.f AOT-SIG-PAYLOAD)
\ through the ARM AOT writer; x86-64 images have no seed pass and no payload.
SUITE aot-effect-pool
   test/aot-effect-pool.f
;SUITE

\ These probes decode emitted code as AArch64 instructions through
\ src/arch/arm64/disasm.f and asm.f (tools/codegen-tail-probe.f,
\ tools/codegen-combine-inventory.f); x86-64 code has no such decoder.
SUITE compiler-codegen-tail-probe
   test/compiler/codegen-tail-probe.f
;SUITE

SUITE compiler-native-fold
   test/compiler/native-fold.f
;SUITE

SUITE compiler-native-vocab
   test/compiler/native-vocab.f
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
