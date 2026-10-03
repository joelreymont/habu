---
title: Add the wasm target row and the scalar-FP bit
status: closed
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.787757+03:00"
closed-at: "2026-10-04T00:48:44.758262+03:00"
close-reason: Wasm arch/ABI and dedicated convention, scalar-FP split and mirrored IR vocabulary implemented. Independent Astra feature/follow-up PASS; actual source-registered floating-HIR observer and explicit emitter refusal pass. Full native suite 592/592 exit0 on source-bound generation5; gen2-5 byte-identical; cold AOT window and old native digest/layout goldens pass. Wasm encoding/runtime remain P6/P7.
---

Problem: CTARGET has no wasm arch or ABI (target.f:52-59, 153-165), and ordinary float HIR demands F-FP, which fuses scalar FP with FMA (hir.f:208-213, ir/type.f:329-335, binding.f:50-53, ir/schema.f:903-913). Both packages are sealed into the engine (forth.md:198-205), so this is the one engine-rebuild commit the Wasm path needs (PA-r2 §4.4, P1a). Acceptance: arch wasm wire code 6 and ABI habu-wasm-cell64-v1 wire code 6 appended; coherence rows (little-endian only, ptr32 and ptr64 descriptions); all encoders and decoders mirrored; scalar-FP bit $200 with F-FP implying it through the capability query; FMA-CK stays on the fused bit; HIR and A64IR schema minors bumped; native literal preimages and digests unchanged. A Wasm ABI on another architecture refuses E-CTGT-ABI; coherent Wasm with no backend answers E-CTGT-UNLOADED; scalar-only targets admit ordinary float schemas and refuse contraction. Verify: rebuilt product, full test/run.f, cold test/aot-wid-build.f, native convergence. Completed prerequisite: habu-pin-native-target-12e4fbb7. Ownership: target.f, binding.f, ir decoders, hir.f, a64ir.f, ir/fun.f and elaborate.f plus their behavior suites. Lane: dave. Claim: dave.

## Bounded sealed change

- The Wasm ABI uses eight-byte Habu cells and pointer storage slots. Ptr32 is P6's implemented address model; ptr64 reserves a coherent memory64 description without claiming support. Wasm is little-endian and permits BASE|SCALAR-FP ($201).
- Add scalar capability to every existing architecture mask. HAS? expands the queried set when its raw F-FP bit is present; the probe and stored raw masks stay unchanged. FEATURES-N, WITH, SAME?, ENCODE and schema-1 identity retain raw semantics.
- Ordinary single/double types and HIR/A64IR float schemas require scalar FP. HIR minor becomes 7 and A64IR minor becomes 15. Native ABI builders retain raw BASE|F-FP; binding contraction still requires F-FP.
- Append IR-FUN's wasm convention at wire code 3. Native Habu/C conventions admit only the existing native architectures, kernel only PTX, and wasm only Wasm. NELAB selects the convention from its binding for definitions, quotations and does> companions.
- Re-derive target-policy enumeration: 7 architectures, 7 ABIs, $400 raw masks, 200704 raw combinations and 1036 legal contracts. Preserve independent enumeration and old literal digest pins; the unknown feature bit becomes $400.
- Exercise target/binding/context/attribute/schema round trips, scalar/fused separation, wrong ABI/endian/convention refusals, the unloaded registry path, and genuine source-tape elaboration/freeze from a fresh package on the sealed product. A test backend may observe folded HIR and refuse emission; it must not substitute native output.
- NBACK/NFROZEN/HIR/NEMIT/NSHADOW already expose the required public hooks and readers. P6 supplies its Wasm capture reader: build-tool aot-shadow's MOVABS rewrite cannot handle LEBs. No native publication or new dispatch framework is part of this leaf.
