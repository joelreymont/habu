---
title: Migrate vector numeric roles
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-13T14:14:22.910415+02:00\""
blocks:
  - habu-migrate-model-ir-c171bdf5
  - habu-seal-cad-num-36dbeec6
---

Full context: lib/vector.f conflates item counts, capacities, and indices and allocates cells through raw adapters. Fix: add exactly VEC:INIT, VEC:CLEAR, VEC:LEN@, VEC:CAP@, VEC:RESIZE, VEC:ENSURE, VEC:@, VEC:!, VEC:PUSH, and VEC:EACH with the effects frozen in MODEL-CAD-V2-PLAN.md B5.5; lengths/capacities use item-count, access uses index, and the private one-cell adapter produces cell-count then alloc-cell-count before MEM:ALLOC-CELLS. Own maki/sched-key.f combined VEC plus all MIR count-accessor migration in that file; tools/lint/intern.f and tools/lint/source-lex.f remain exclusively in the tool allocation wave. Acceptance: zero length valid, zero capacity allocation rejects, growth overflow and index/count swaps reject, sched-key output exact, runtime bound/generation evidence remains bounded-host ownership. Files: lib/vector.f, lib/vector-test.f, maki/sched-key.f, maki/sched-key-test.f. Depends on sealed CAD-NUM, packaged MEM, and MIR count APIs.


VEC SURFACE LANDED 2026-07-15 (vecnum worker, "lib: add typed VEC surface over
CAD-NUM roles", merged bead646b): all ten B5.5-frozen VEC: words in package VEC
wrapping the unchanged raw VEC-* words; CAP-ALLOC one-cell adapter (item-count
-> cell-count -> alloc-cell-count -> MEM:ALLOC-CELLS) with E-VEC-CAPACITY at
the owner boundary (MEM:CELLS-ALLOC-COUNT throws E-MEM-SIZE, wrong boundary
code - restated narrowing instead); two private audited projections
ITEM-COUNT>N/INDEX>N (TRUSTED.md rows, epic owner). Zero-length valid,
zero-cap/overflow reject, index/count swaps reject statically. REMAINING (why
this dot is re-gated on habu-migrate-model-ir-c171bdf5): the sched-key
migration must be ONE combined VEC+MIR-count commit keeping every rendered-key
pin exact, and the typed MIR accessors (MIR:NODE-COUNT@/OPERAND-COUNT@) do not
exist yet - c171bdf5 owns them. maki/sched-key.f deliberately untouched
(byte-for-byte, pins green).
