---
title: Keep DEFLINEAR callable after a source reset
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:27:33.793480+02:00"
---

Problem: the checker-unit-codec row calls DEFLINEAR (src/core/roles.f:163) at top level after OWNER-RESET:SOURCE; the reset drops the source-owned rows, so no row answers DEFLINEAR, and once TOP-JUDGE refuses a top-level step without a row the row goes red (census of habu-refuse-a-top-57851ac2: the one refusal that is not set-tier; a 3-line probe reproduces it; inside a body the call is already refused on master). Rule: a reset leaves the dictionary coherent: either the words whose rows it drops are gone with them, or their rows stay; a test may not call a defined word that has no row. Acceptance: the contract of OWNER-RESET:SOURCE (its header) decides which side is wrong, the fix is at that layer (the reset, or the test calling before the reset or re-requiring roles), and the row passes on the engine that carries the TOP-JUDGE rule; no special case for DEFLINEAR. Files: the checker-unit-codec test file, src/core/roles.f or the reset's owner. Verify: that row on the rule's engine; the probe. Depends: the TOP-JUDGE rule lane in carl-topjudge. Ownership: the reset's owner. Claim: unassigned.
