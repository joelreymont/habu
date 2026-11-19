---
title: Reject a scratch family bound to a stored cast type
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:43:46.068319+01:00"
---

Problem: src/core/checker.f TV! (STORED-TERM-HIT) checks a type bound to a cast's stored variable for a linear value, an atom and a width only, so `CAST: PEEK ( ^t -- ^n )` in a private section, called outside AIO with t bound to AIO:ticket, does what the closed `( ^AIO:ticket -- ^n )` is rejected for (E-CAST-SCRATCH). type-family.f, which knows a family is SCRATCH-ONLY, loads after the checker, and a hook is machinery being replaced (docs/design.md): the family's owner records the data the checker reads. Acceptance: such a binding outside the family's package is E-CAST-SCRATCH and inside it is admitted; test/compiler/stored-type-param.f holds both. Depends: none. Claim: unassigned.
