---
title: Carry intact-input evidence through a callee locals frame
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T17:09:36.829956+03:00"
---

Problem: the callee mask (commit 63ddfa08, "Carry a callee's intact inputs across catch"; src/core/checker.f:5236-5262, THROW-EDGE at :5852) records which declared inputs every throw path of a word leaves in place, so a caller's catch keeps them typed. A callee that binds its inputs to locals first records nothing intact: the locals frame opens a fresh base row and the mask is measured against it. test/catch-stale-suite.f:42 WBIND (`{: p :} p drop E-CS-BOOM throw`) pins this, with cases C4 (:307) and T6 (:454) asserting the caller's cells stale. This is conservative, not wrong; such callers keep binding their values to locals before the catch. The callee-mask work counted 18 such callees among the 39 it migrated; its list is gone. The proposed model: a bound input the body never writes back over is intact at every throw, so the mask can be computed over the definition's declared input window (SGIN/SGRIN) before the frame is entered.

Acceptance: first, confirm at the locals-frame code and at catch's stack restore that the runtime leaves a bound input's data-stack cell untouched until the body's stack grows back over it, and say where. Then a locals-bound input whose cell the body does not overwrite before a throw is recorded intact, so `[: WBIND ;] catch` keeps ( ptr u8 ) typed (C4 and T6 certify), while a body that pushes over the vacated cell before throwing (`{: p :} 1 E-CS-BOOM throw`) stays stale. The report lists the tree callers whose pre-catch locals the change makes unnecessary; converting them is not part of this dot.

Files: src/core/checker.f, test/catch-stale-suite.f, docs/forth.md (the catch paragraph 63ddfa08 added).

Verify: test/catch-stale-suite.f with C4, T6 flipped and the overwrite case added; test/run.f.

Depends: none.

Ownership: unassigned. Claim: unassigned.
