---
title: "CAD: ADT swap for report/IR/schedule internals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:50.079838+02:00"
---

docs/model-cad.md typed backbone. When TFAM 9/10/12/14/15 land: swap cad-0a report, cad-1 IR, cad-4 schedule internals to sum/enum/product families with MATCH dispatch (op-kind enum, verdict sum pass|fail<reason>, fusion sum fused|split<reason>, report rows as products, option/result for lookups). Representation-hiding accessor signatures must not change; tests prove behavior identical. Recursive by-value IR waits for TFAM 16 boxed (habu-epic-adopt-adts child). Depends: TFAM campaign on maki-type-families.
