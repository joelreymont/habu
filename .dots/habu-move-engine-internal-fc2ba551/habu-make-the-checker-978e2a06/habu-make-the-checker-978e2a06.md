---
title: "Make the checker's global internals private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.889423+01:00"
---

Problem: checked code still reaches checker machinery: a program states checker facts (PRIM: rows, `n INTRINSIC` on its own word, NOMINAL-PROVIDER, an undefine of an engine word; b93eb0c0), calls the global words of src/core/include.f (c0423387) and six public TYPE-DECL words (c71b7e67). Acceptance: its children are closed. Depends: none. Claim: unassigned.
