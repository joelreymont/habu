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

\ These read source definitions as fixed-width AArch64 instructions. The
\ identity-spill and wide-frame cases inspect SP slots; fused-moves inspects
\ indexed x19 transfers. Their runtime subjects are checked in those suites.
SUITE compiler-native-identity-spill
   test/compiler/native-identity-spill.f
;SUITE

SUITE compiler-native-wide-frame
   test/compiler/native-wide-frame.f
;SUITE

SUITE compiler-native-fused-moves
   test/compiler/native-fused-moves.f
;SUITE

\ The producer builds an ARM source-recovery host. The capture format's
\ portable storage and transfer cases remain in the shared capture row.
SUITE aot-chain-producer
   test/aot-chain-producer-suite.f
;SUITE

\ Live ARM windows and instruction-chain compaction need an ARM host. Inert
\ metadata storage, transfer and refusal cases stay shared.
SUITE aot-chain-capture
   test/aot-chain-capture-suite.f
;SUITE

\ These capture live ARM source windows or inspect their four-byte instruction
\ sites. Inert artifact storage, transfer and refusal checks remain shared.
SUITE aot-wide-format
   test/aot-wide-format-suite.f
;SUITE

SUITE aot-wide-prefix
   test/aot-wide-prefix-suite.f
;SUITE

SUITE aot-wid-restore
   test/aot-wid-suite.f -- restore
;SUITE

SUITE aot-wid-refuse-wid0
   test/aot-wid-suite.f -- refuse-wid0
;SUITE

SUITE aot-wid-refuse-address-span
   test/aot-wid-suite.f -- refuse-address-span
;SUITE

SUITE aot-wid-boot-sealed
   test/aot-wid-suite.f -- boot-sealed
;SUITE

SUITE aot-wid-rebase
   test/aot-wid-suite.f -- rebase
;SUITE

SUITE aot-wid-capture-refusal
   test/aot-wid-suite.f -- capture-refusal
;SUITE

SUITE aot-data-window
   test/aot-data-window-suite.f
;SUITE

SUITE aot-capture-compact
   test/aot-capture-compact.f
;SUITE

SUITE aot-named-cells
   test/aot-named-cells.f
;SUITE

SUITE aot-prelude-band
   test/aot-prelude-band-suite.f
;SUITE

SUITE aot-payload-graph
   test/aot-payload-graph.f
;SUITE

SUITE aot-prefix-literal
   test/aot-prefix-literal.f
;SUITE

SUITE aot-effect-pool
   test/aot-effect-pool.f
;SUITE

\ These cases exercise the ARM compact-blob planner, four-byte trailers and
\ ADR extent inference. Shared create/does and stripped-image rows still run.
SUITE compiler-native-code-span
   test/compiler/native-code-span.f
;SUITE

SUITE compiler-aot-nested-body
   test/compiler/aot-nested-body.f
;SUITE

\ Mutations target the ARM boot reader's packed ULEB record/site tables.
\ The ordinary seed-batch build and execution row stays shared.
SUITE aot-seed-metadata
   test/aot-seed-metadata.f
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
