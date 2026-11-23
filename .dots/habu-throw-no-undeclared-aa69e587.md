---
title: Throw no undeclared code in the number tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:36:20.282202+02:00"
---

Problem: src/lib/f64-text-test.f:15,19 and test/interpreter-number.f:43 throw the literal -9899 when a parse or format answers err; no ERROR:CODE declares it (docs/forth-card.md: a code is declared with its text), and it sits in the unclaimed block -9890..-9899. Acceptance: each of these failures is reported through the test library (src/lib/test/assert.f) or a declared code. Depends: none. Claim: unassigned.
