---
title: Row or remove the two undeclared engine primitives
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T18:17:40.772050+03:00"
---

Problem: run-rc (src/habu/habu1.f:3313) and top-check@ are registered engine primitives with no checker row and no declaration anywhere (measured 2026-09-17 by the primitive-table lane: ': RC-TEST ( -- n ) ... run-rc ;' answers E-UNDEFINED), so no checked program can name them and the bodies are dead surface in the image; the table src/habu/prims.f (habu-specify-the-engine-fcbcee25) lists them as UNROWED with that reason so its completeness gate stays explicit. Acceptance: for each, either a real row (top-check@ matching check@'s row with PRIM-TRUSTED-ONLY!, since its writer set-top-check is trusted-only, checker.f:7095; run-rc's effect taken from its body) with a test that a checked program can call it, or the body and its registration removed with the callers audited; the UNROWED kind disappears from the table when both are settled. Files: src/habu/prims.f, src/habu/habu1.f, src/core/checker.f (elaboration if any), test/. Verify: the completeness gate in the engine build; byte fixpoint; test/run.f. Depends: habu-specify-the-engine-fcbcee25. Ownership: engine primitives. Claim: unassigned.
