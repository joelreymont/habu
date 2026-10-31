---
title: Decode base64 with a lookup table
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:25:44.785693+02:00"
---

BASE64:DECODE takes about 5.0 s for 50 MiB (phil, Zen's MCP upload lane, 2026-10-31: a 50 MB upload took 9.3-14.5 s end to end, ~5.0 s of it the decode). src/lib/base64.f:37 CHAR>SEXTET does a linear INDEX-OF over the alphabet, twice per input character. A 256-entry table makes it one load per character. Measure the decode of a 50 MiB input before and after. No Zen change depends on it. Ownership: carl. Claim: unassigned.
