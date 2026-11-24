---
title: "Stop loudly on a damaged object's facts"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.566911+01:00"
closed-at: "2026-11-24T17:18:19.570781+01:00"
---

Problem: a saved object's type entry is read lazily after its link (src/core/type-family.f ER-READ, ER-HEAD, ER-BODY, ER-FIELD, ER-NODE), and an undefine's text during it (src/core/checker.f READ-UNDEFINE). A malformed text ends the build in an internal die (ER-BAD, READ-UNDEFINE's, or TFAM-DECL's, TFAM-PK!'s, TFAM-LAYOUT!'s, LAY-ADD's and PF-ADD's range dies) that names no damaged object and no fix. docs/architecture.md "Builds": a damaged object stops the build. Acceptance: (1) each malformed text ends the build in one E-DAMAGED-OBJECT record naming the type or word and how to recover, exit DATAERR; (2) the reader checks each number's range once where it reads it, and the declaration words' dies that only the reader could reach go. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-objbad.
