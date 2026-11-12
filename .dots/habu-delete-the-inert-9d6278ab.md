---
title: Delete the inert CHECKER-SWEEP package
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:27:56.598549+02:00"
---

Problem: src/core/checker.f CHECKER-SWEEP (:29838-30140) acts only after INSTALL (:30107) sets KEEP-SET, and nothing calls INSTALL since src/core/checker-surface.f went (it was required by nothing before that), so SWEEP (:30116, called at :30235) and STRIP (:30126) return at once and NAMED?, KEEP-XT, NAMED-XT and what only they reach are dead; the comment at :804-814 still describes the installed policy. src/habu/aot-capture.f:2089 reaches STRIP by name from a string. carl agreed (2026-11-12) that this dot takes STRIP from habu-remove-the-public-437609bd. Acceptance: the package, aot-capture.f's lookup of STRIP and what only they reach are deleted, STRIP is cut from 437609bd, the comment says what the capture keeps without it, and the engine rebuilds to a fixpoint. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/checker-sweep.
