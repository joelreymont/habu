---
title: Add the wasm target row and the scalar-FP bit
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.787757+03:00"
---

Problem: CTARGET has no wasm arch or ABI (target.f:52-59, 153-165), and ordinary float HIR demands F-FP, which fuses scalar FP with FMA (hir.f:208-213, ir/type.f:329-335, binding.f:50-53, ir/schema.f:903-913). Both packages are sealed into the engine (forth.md:198-205), so this is the one engine-rebuild commit the Wasm path needs (PA-r2 §4.4, P1a). Acceptance: arch wasm wire code 6 and ABI habu-wasm-cell64-v1 wire code 6 appended; coherence rows (little-endian only, ptr32 ok); decoders mirrored in ir/attr.f:310-328, ir/context.f:305-328, ir/schema.f:357-362, 405-413, 460-475 as 35094f08 did; a new scalar-FP bit $200 with F-FP implying it; FMA-CK stays on the fused bit; HIR schema minor bumped; literal digests from habu-pin-native-target-12e4fbb7 unchanged; domain counts re-derived deliberately. Files: src/compiler/target.f, src/compiler/binding.f, src/compiler/native/hir.f, src/compiler/ir/{attr,context,schema,type}.f, test/compiler/{target-policy,target-registry,ir-attr,ir-context,ir-schema,ir-type}.f. Verify: full gate (product candidate, test/run.f, cold aot-wid-build); a wasm ABI on a non-wasm arch refused E-CTGT-ABI; a wasm contract with no backend answers E-CTGT-UNLOADED; a float schema admitted under the scalar bit alone; contraction under it alone refused E-CBIND-CONTRACT. Depends: habu-pin-native-target-12e4fbb7. Ownership: src/compiler/target.f, binding.f, ir decoders. Lane: dave lands it; tim specifies it, since it is the one shared edit on the Wasm path. Claim: unassigned.
