---
title: Refuse a too-deep inferred effect by its depth, not as a cyclic term
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:46:51.682181+02:00"
---

Problem: an undeclared word whose inferred output nests about 8294 pointer levels (DEEP trusted at 4094 levels, then 4200 WRAPs; reproducer ~/.cache/tmp/rv-semiq2/deep3-repro.f) dies rc 76 'checker: term walk too deep (cyclic term)' instead of E-UNCHECKABLE 'effect too deep to record': the per-token term walk hits TWALK-MAX-DEPTH (src/core/checker.f:3064, :3074) before CHECK-DEPTH-FITS sees the effect, and the term is not cyclic, so the message is false. Same on the installed engine and with the semiq commit. Found by the semiq re-review. Acceptance: the reproducer is refused E-UNCHECKABLE (or another diagnostic that names the depth, not a cycle) with a rc the checker's refusals use; a genuinely cyclic term still says cyclic. Files: src/core/checker.f. Verify: the reproducer through hb --load and tools/check.f. Depends: none. Ownership: src/core/checker.f term walk. Claim: unassigned. Waits behind the consumer-met work (AGENTS.md: work that does not move the consumer measure waits).
