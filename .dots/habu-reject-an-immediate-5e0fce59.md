---
title: Reject an immediate that marks no program definition
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:01.484254+01:00"
---

Problem: ENUM, STRUCTURE, MUTUAL, SUMTYPE and EXPORT do not call NAME-DECLARED, so an `immediate` after one marks different words in the load and in verify-source.f's pre-pass; one before any program definition marks an engine word (HBB-MAIN). Acceptance: the load and the pre-pass reject such an `immediate` alike, with a record naming the declarer; after an undeclared renderer it stays the run's (TOP-OPAQUE). Depends: none. Claim: unassigned.
