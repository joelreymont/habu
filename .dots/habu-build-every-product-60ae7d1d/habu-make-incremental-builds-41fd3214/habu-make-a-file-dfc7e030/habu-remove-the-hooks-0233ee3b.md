---
title: Remove the hooks no word sets any more
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T00:32:07.886455+02:00"
---

Problem: the per-definition build cache's setters are removed, so these hooks keep only their default and still run on every use. The publication slot and its readers: src/core/checker.f RECORD-PUBLISHED's read (:15430), src/habu/habu2.f NOTIFY-PUBLICATION (called at :857), src/core/checker-owner-abi.f PUBLICATION-USE-OFF (:159), src/core/checker-owner-extension.f:12. Acceptance: each hook is replaced by its default's plain call or deleted with the code that only it reaches; no public defer or xt cell of the deleted cache remains. Before a brief: a census of public words whose body stores a defer (is), an xt cell or a hook variable and that have no caller in Habu, Zen, Tender and Maki, to end the one-by-one finds; list each hook's callers. A program can still rebind any public defer with is: that is habu-seal-the-checker-a751051f. Depends: none. Not on the release path: no consumer meets it. Claim: unassigned, owner carl.
