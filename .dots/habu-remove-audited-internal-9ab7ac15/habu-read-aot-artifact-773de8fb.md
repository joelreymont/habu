---
title: Read AOT artifact payloads once
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.800386+03:00"
---

aot-file.f VERIFY-PAYLOAD streams/hash whole file, then LOAD-PASS closes/reopens and hashes again. Keep one descriptor: validate bounded header/table, load/hash sections once, check digest/EOF before semantic restoration. Preserve size/budget/address/producer/dependency checks and cleanup on refusal. Existing real artifact read/merge/roundtrip and malformed-artifact tests, independent review and capture checks.
