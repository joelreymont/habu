---
title: Intern compiler symbols
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:38.137128+02:00\""
closed-at: "2026-08-15T14:07:28.845237+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): symbol interning (canonicalizer clause superseded CG-31). Production-consumed by the native chain; suites dual-registered, green through the real entry."
---

Full context: design section 6.3 requires deterministic string and symbol tables before type/schema records. Add geometric-growth byte/string storage and symbol interning with module-local IDs, byte equality, and no pointer identity. Acceptance: duplicate bytes return one ID; insertion order is removed by later canonicalization metadata; capacity/overflow/foreign-owner negatives pass. Dependency: compiler source registry (landed as src/compiler/ir/source.f; the earlier frontmatter edge pointing this dot at that dot was inverted and stale, so it is removed).

Claim: agent=ir-sym workspace=.jj-ws/habu-intern-compiler-symbols-ed025e24
