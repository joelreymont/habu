---
title: Pass records with float members by value
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T00:00:48.508697+01:00"
---

Problem: STRUCT-ARG and STRUCT-RESULT (src/lib/ffi-abi.f, the FUNCTION: header) take a byte count only, so a record with float members (an HFA or HVA, passed in v registers) or one aligned to 16 bytes cannot be declared: a C function taking or returning a struct of doubles (CGPoint, CGRect) cannot be called. Acceptance: a record declared by its type passes and returns by value by AAPCS64's and Apple's rules, an HFA in v registers; a test calls a C function taking and returning a struct of two doubles. Depends: none. Claim: unassigned.
