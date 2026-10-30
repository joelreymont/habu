---
title: "T0: a counted string over 255 bytes throws E-T0-LITERAL without a message"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.323923+02:00"
---

Problem: c" <256 bytes>" in a definition: tier 1 says 'counted string too long (max 255)' rc 76; T0 throws E-T0-LITERAL -9602 at src/habu/t0.f:1223 with no message, rc 67 (program .jj-ws.noindex/jerry-s2/.out/gaps/counted-long.f; suite sort class d, 2026-10-30). Acceptance: T0 refuses with tier 1's message and rc, a failing test first. Files: src/habu/t0.f. Verify: the program's output and rc match tier 1's. Depends: none. Ownership: t0.f literal path. Claim: unassigned.
