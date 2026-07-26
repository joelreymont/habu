---
title: Intern compiler symbols
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:38.137128+02:00"
blocks:
  - habu-register-compiler-sources-fd495290
---

Full context: design section 6.3 requires deterministic string and symbol tables before type/schema records. Add geometric-growth byte/string storage and symbol interning with module-local IDs, byte equality, and no pointer identity. Acceptance: duplicate bytes return one ID; insertion order is removed by later canonicalization metadata; capacity/overflow/foreign-owner negatives pass. Dependency: compiler source registry.
