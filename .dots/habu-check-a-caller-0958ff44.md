---
title: "Check: a caller of a left word that moves ^t"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:23:50.533040+01:00"
---

Problem: a word the check leaves to the run has no judged kinds, so a caller that hands it ^pt where its body fetches through ^t is accepted by the check while the load rejects it (src/core/check-walk.f CHECKER-USIG-CERT-ADD-PENDING). Acceptance: the check defers such a caller (W-CHECK-DEFERRED) with a kind for a left row beside STORED and MOVED. Depends: 601e92b3 (a left call is recorded and judged last). Claim: unassigned.
