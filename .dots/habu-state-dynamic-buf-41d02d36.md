---
title: "State DYNAMIC-BUFFER's geometric growth as its contract"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:37:05.503188+02:00"
---

Problem (tim for Maki, 2026-11-07): docs/forth.md:2112-2113 promises only that `count NAME-RESERVE` allocates at least that many elements, but DYNAMIC-STORAGE CAPACITY (src/core/dynamic-storage.f:44-46) grows to at least twice the old capacity with a 64-byte minimum (up to MAX-BYTES/2), and Maki's src/ops.f reserves one element per append and relies on that to stay linear (1M appends in 0.4 s, tim's measure). An undocumented growth rule can be changed to exact growth without any caller's test failing, turning those appends quadratic. Acceptance: the forth.md DYNAMIC-BUFFER passage states that a growing reserve at least doubles the capacity, so one reserve per append costs amortized constant time; CAPACITY's comment states the same rule as a contract; one end-to-end case that would catch exact growth (e.g. a timed or counted growth check through a DYNAMIC-BUFFER's capacity, not a change detector of the 64-byte minimum). Files: docs/forth.md, src/core/dynamic-storage.f, the dynamic-buffer test. Verify: the case. Depends: none. Ownership: carl. Claim: unassigned.
