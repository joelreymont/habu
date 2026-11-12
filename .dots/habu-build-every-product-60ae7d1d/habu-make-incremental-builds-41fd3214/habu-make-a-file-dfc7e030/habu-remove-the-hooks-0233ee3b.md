---
title: Remove the hooks no word sets any more
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T00:32:07.886455+02:00"
---

Problem: the per-definition build cache's setters are removed, so these hooks keep only their default and still run on every use. The publication slot and its readers: src/core/checker.f RECORD-PUBLISHED's read and TRANSFER-PUBLICATION (:27031), src/habu/habu2.f:776-785 NOTIFY-PUBLICATION (called at :914), src/core/checker-owner-abi.f:265 PUBLICATION-USE-OFF, src/core/checker-owner-extension.f:24. The hook defers and default bodies: TFAM FAMILY-KEY-XT, FAMILY-BY-KEY-XT, FAMILY-KEY-MATCH-XT, CON-DECLARED-XT, CON-KEY-XT, CON-BY-KEY-XT, CON-BIND-XT and FAMILY-HOOK-DEFAULTS (src/core/type-family.f:22-47); CHECKER-REG:CON-DECLARED-XT (checker.f:16867-16875); UNIT-CON-KEY$, UNIT-CON-BIND and FILE-CON-RESET (checker.f:27130-27150). Acceptance: each hook is replaced by its default's plain call or deleted with the code that only it reaches; no public defer or xt cell of the deleted cache remains. Before a brief: a census of public words whose body stores a defer (is), an xt cell or a hook variable and that have no caller in Habu, Zen, Tender, Maki and Etch, to end the one-by-one finds; list each hook's callers. A program can still rebind any public defer with is: that is habu-seal-the-checker-a751051f. Depends: none. Not on the release path: no consumer meets it. Claim: unassigned, owner carl.
