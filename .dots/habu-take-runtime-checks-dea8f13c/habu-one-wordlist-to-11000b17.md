---
title: One wordlist-to-package lookup
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T15:46:28.368616+01:00"
---

Problem: src/core/checker.f REC-HOME (the package and section owning a record's wordlist, a linear scan of every record) and src/core/check-walk.f REC-SYM over NS-FIND (the same answer through an index) are two lookups of one fact. Acceptance: one lookup, the indexed one, answers both callers' questions; REC-HOME's scan is gone. Depends: none. Claim: unassigned.
