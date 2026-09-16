---
title: Reject trailing tokens in stack-effect signatures
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-14T13:31:34.150268+03:00\""
closed-at: "2026-09-16T14:34:49.725434+03:00"
close-reason: "superseded by habu-campaign-c4-diagnostics-3b6de147: Residue: the signature parser must consume the whole grammar and reject trailing separators and tokens."
---

Audit M8 independently reproduces on current candidate SHA46921f52: : F ( n -- n -- n n ) ; : G ( n -- n ) F ; 1 G . certifies and runs. PSIG around checker.f3794 must consume the complete signature grammar and reject trailing separators/tokens. Preserve valid return-row syntax, named rows and malformed effect diagnostics. No patch at restart.
