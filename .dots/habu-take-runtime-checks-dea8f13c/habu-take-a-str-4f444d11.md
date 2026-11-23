---
title: Take a str in CHECK! and its kin
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:12:11.763944+01:00"
---

Problem: src/core/checker.f CHECK-RESET dies 'token buffer too large' (EXIT-STATUS:INTERNAL) and a program reaches it: test/name-length-test.f (CHECK!-IT, CANDIDATE-IT, HOOK-IT) passes length -1 or the largest cell to CHECK!, CHECK-CANDIDATE! and CHECK:DEFINITION and expects that die (rc 76). They take ^u8 n (Joel 2026-11-19: no ^u8 n, a str or a vector). Acceptance: the three take a str; CHECK-RESET holds no internal die; name-length-test asserts the new behaviour. Depends: none. Claim: unassigned.
