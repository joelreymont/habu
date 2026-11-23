---
title: Mark a deferred definition by its own name
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:34:39.946113+01:00"
---

Problem: a top-level definition the check defers for its signature marks its whole wordlist unseen (src/core/check-walk.f:1462 CHECKER-UNSEEN-MARK), so every later unknown word at top level is deferred instead of E-UNDEFINED: the global mark the BUFFER feature removed from declarations. Acceptance: a deferred definition covers only the words it can define, and a misspelt word after it at top level is E-UNDEFINED. Depends: the per-name mark (carl-bufdt, BUFFER count feature). Claim: unassigned.
