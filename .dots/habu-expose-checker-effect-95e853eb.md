---
title: Expose checker effect-read API to the prefix
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T21:31:32.013095+02:00\""
---

Found by the toprow lane 2026-07-14: the tier-1 top-row tracker (src/core/top-row.f) cannot read per-word din/dout effect families because the checker's effect-read internals (E-PTR, ER.DIN, CHECKER-FIND-ACTIVE-SYM, PE-EFF@, EN-*) are name-stripped/treeshaken from the baked engine that a cold-prefix file sees - so it uses a coarse family shadow (scalar/pointer/xt/gray + literal/tick classes + certified min-in flags). Fix in the checker/compiler: export a stable, minimal effect-read API to the prefix (keep-name the needed words or add a compact query primitive) so tier-2 (habu-typed-top-tier-589c550f, pre-execution reject with real row unification) can be built. Acceptance: a cold-prefix file resolves the API and reads a certified word's din/dout families; treeshake keeps it across the fixpoint (byte-identical x2); a negative regression pins the API surface. Files: src/habu/habu2.f keep-name/tree-shake tables or src/core checker export, src/core/top-row.f consumer, checker tests. Verify: fixpoint x2, full run.f, top-row-warn-test. NOTE: engine/AOT territory - coordinate with the owner-persist lane before claiming (habu2.f sections).

Claim: agent=effread workspace=.jj-ws/fable-toprow
