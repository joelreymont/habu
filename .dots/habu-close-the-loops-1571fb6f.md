---
title: Close the loops clang closed-forms
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T13:24:42.724307+02:00"
---

The real owner of TINY-CALLEE 80 vs 12 (refuted-inlining lane, 2026-08-07, reproduces in 11s): the chain already inlines everything there (BL-COUNT 0) and already has the derived per-site copy rule (inline.f SMALL? = measured body <= in+out+3); the gap is clang computing the closed form seed+4*len branchlessly — induction-variable strength reduction / closed-forming, an optimization class the chain lacks entirely. Likely siblings: SUM-TO (n(n+1)/2), CALL-LOOP-3, MANY-LOCALS — the mechanism-attribution audit (running) confirms the set. Measure-first: name the rows from the audit, the transform operates on the typed IR where induction facts must first SURVIVE the elaborator (the NEON lane recorded that trip counts die in compile-time stacks — that loop-shape work is shared with the vectorizer and lands once), direct-refinement evidence per fold, all answers bit-for-bit incl. boundary trip counts (0, 1, MAX).

Audit confirmation (2026-08-07): the closed-formed set is SUM-TO (Gauss via 128-bit multiply + funnel shift — disassembly in the audit), COUNT-DOWN (csinc), TINY-CALLEE (bic;add lsl#2 — 56 of its 72 gap bytes), MANY-LOCALS (madd by len — byte-neutral but its whole 7.97ns), STORE-LOAD (3*len), PRESSURE-LOOP (sum*len). 112 gap bytes, top of the time list.
