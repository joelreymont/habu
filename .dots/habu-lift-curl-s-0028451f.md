---
title: "Lift curl's 64 KiB text cap"
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T20:01:49.153480+02:00"
---

src/lib/net/curl.f:124 `$10000 constant MAX-TEXT` with WITHIN-RANGE at 271, 504, 520, 534, 555 refuses a URL, header line or string option over 64 KiB with E-OPERAND. The owner rule forbids a fixed input cap without his decision. Fix: size each copy to its text (curl copies strings itself). Ruling (carl, best long term): lift it; no text length is refused. Found by the batch 2 review (vmsvlupr).
