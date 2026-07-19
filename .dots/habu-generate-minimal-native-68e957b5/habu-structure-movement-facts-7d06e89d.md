---
title: Structure movement facts
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:22:18.729986+02:00"
---

maki/move-facts.f:34-167 exposes movement transform and dissolution verdict as two raw n domains packed with two parameters into one cell. MV-PACK accepts four interchangeable integers; valid cross-domain swaps such as transform=MV-CONCAT and verdict=MVV-MATERIALIZE becoming transform=MV-SLICE and verdict=MVV-GATHERED pass every range check. No-parameter transforms can also carry ignored nonzero fields. MV-TF@/MV-VD@ return n, so all downstream APIs repeat runtime checks and comparison chains. Keep the compact 64-bit attrs cell as an explicit encode/decode boundary, but model decoded facts with a payload ENUM transform whose reshape/slice variants carry their typed row/column fields, a closed verdict ENUM, and a STRUCTURE movement-fact containing the two semantic fields. Encode validates field widths once and writes ordinals; decode rejects reserved/high bits and constructs the typed value; all classifiers/reporting use exhaustive MATCH. Retire public raw MV-PACK/MV-TF@/MV-VD@ and raw verdict returns; wire ordinals remain private named constants only at the codec. Add checker negatives for transform/verdict/extent swaps and impossible payloads, mutation tests for every tag, reserved bits, max/overflow params and invalid combinations, plus exact encode/decode goldens and round trips. Preserve the attrs ABI, planner decisions, report bytes, and emitted kernels. Measure JIT/DATA/CODELEN and classification throughput before/after. Files: maki/move-facts.f and its direct model-ir/plan/report tests. Verify focused movement/lowering suites, Maki, typed-local diff, type/package/host/filemap/dot lints, and full native gate. Ownership: decoded movement-fact representation and attrs codec only.
