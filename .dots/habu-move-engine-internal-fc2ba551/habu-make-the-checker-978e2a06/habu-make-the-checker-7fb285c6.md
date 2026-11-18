---
title: "Make the checker's global internals private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T16:11:36.409573+01:00"
---

Problem: the CHECK words in src/core/checker.f's global sections listed in the parent (LOAN-FIELD-FAM through CHECKER-EXPORT-DECL, TV-ENSURE through EVENT-COPY-PATH, CHECKER-DEFLINEAR, CHECKER-DEFRECORD, TFAM-ARITY-XT and the other checker defers) are global, so a program calls them or rebinds a defer (`: P ( -- ) [: drop 0 ;] is TFAM-ARITY-XT ;` loads, rc 0). Acceptance: each is E-UNDEFINED outside CHECK; engine callers take it by EXPORT; no checker defer can be rebound outside its package. Depends: none. Claim: unassigned.
