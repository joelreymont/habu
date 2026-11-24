---
title: Drop the entry guard's shared-entry allowance
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:15:59.605240+02:00"
---

Problem: test/gate-entry-guard.f lets two suite rows share an entry file (:8 "and rows may share an entry", ROW-B :19, set in ENTRY-ADD :38, read in OTHER :43); a census found no row that shares one. Acceptance: if no two rows in the suite's table share an entry, ROW-B and the allowance go and the guard refuses a shared entry; otherwise the row that needs it is named beside the allowance. Depends: none. Claim: unassigned.
