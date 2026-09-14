---
title: Reject trailing tokens in stack-effect signatures
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T13:31:34.150268+03:00"
---

Audit M8 independently reproduces on current candidate SHA46921f52: : F ( n -- n -- n n ) ; : G ( n -- n ) F ; 1 G . certifies and runs. PSIG around checker.f3794 must consume the complete signature grammar and reject trailing separators/tokens. Preserve valid return-row syntax, named rows and malformed effect diagnostics. No patch at restart.
