---
title: Read AOT artifact payloads once
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.800386+03:00"
closed-at: "2026-10-22T12:36:35.147937+03:00"
close-reason: "Triage 2026-10-22, DONE: src/habu/aot-file.f:1053-1063: LOAD-PASS reads and hashes each section through one descriptor. VERIFY-PAYLOAD is gone."
---

aot-file.f VERIFY-PAYLOAD streams/hash whole file, then LOAD-PASS closes/reopens and hashes again. Keep one descriptor: validate bounded header/table, load/hash sections once, check digest/EOF before semantic restoration. Preserve size/budget/address/producer/dependency checks and cleanup on refusal. Existing real artifact read/merge/roundtrip and malformed-artifact tests, independent review and capture checks.
