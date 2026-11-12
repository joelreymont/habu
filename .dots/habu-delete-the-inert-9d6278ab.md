---
title: Delete the inert CHECKER-SWEEP package
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:27:56.598549+02:00"
---

Problem: src/core/checker.f CHECKER-SWEEP (:29838-30140) acts only after INSTALL (:30107) sets KEEP-SET, and nothing calls INSTALL since src/core/checker-surface.f went (it was required by nothing before that), so SWEEP (:30116, called at :30235) and STRIP (:30126) return at once and NAMED?, KEEP-XT, NAMED-XT and what only they reach are dead; the comment at :804-814 still describes the installed policy. Acceptance: the package and what only it reaches are deleted, the comment says what the capture keeps without it, and the engine rebuilds to a fixpoint. Depends: none. Claim: unassigned.
