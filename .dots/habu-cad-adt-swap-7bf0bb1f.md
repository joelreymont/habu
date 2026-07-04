---
title: "CAD: ADT swap for report/IR/schedule internals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:50.079838+02:00"
---

docs/model-cad.md typed backbone. When TFAM 9/10/12/14/15 land: swap cad-0a report, cad-1 IR, cad-4 schedule internals to sum/enum/product families with MATCH dispatch (op-kind enum, verdict sum pass|fail<reason>, fusion sum fused|split<reason>, report rows as products, option/result for lookups). Representation-hiding accessor signatures must not change; tests prove behavior identical. Recursive by-value IR waits for TFAM 16 boxed (habu-epic-adopt-adts child). Depends: TFAM campaign on maki-type-families.

UPDATE 2026-07-04 (user review of cache-key asserts): the section-7.4 schedule
key is the sharpest instance of the gap. Today maki/sched-key.f SK-KEY$ renders
eight semantically distinct fields (region-sig, shape-class, dtype, layout,
align-class, target, engine-key, ptxas) straight into the shared SB builder as a
pipe-delimited string; the replay table and the store key by that string, and
tests must assert rendered text. Stringly fields are a semantic-role hole: dtype
and layout are indistinguishable bytes, so a field swap is silent. Required
shape when TFAM 14 (ENUM) + 15 (PRODUCT) land: SKEY as a product record with
enum fields (DT-*, LAY-*, AL-* constants already exist to become real enums),
typed constructor + field accessors, typed equality for the replay table, ONE
render word at the durable-store boundary (schedules.rows stays line-oriented
text - the on-disk format is a contract with exactly one format regression
test), and key tests move from string asserts to field asserts. Same treatment
for the report gate tags (verdict sum) and evidence rows (product).
