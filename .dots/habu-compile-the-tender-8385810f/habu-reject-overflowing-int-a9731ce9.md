---
title: Reject overflowing integer literals in engine and checker admission
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-14T14:46:34.531945+03:00\""
closed-at: "2026-09-16T14:34:49.728816+03:00"
close-reason: "done: Engine and checker admission both refuse overflowing decimal and hex literals, with pinned fixtures. [test/compiler/integer-literals.f REFUSALS and CHECKER-ADMISSION (IL-OVER, IL-WRAP, IL-HEX-WRAP expect verdict 0).]"
---

M4 reproduced with signed decimal limits and 2^64 decimal/hex wrapping in interpret and compile; CHECK-CANDIDATE! also certifies impossible integers. Engine fix 0965cd27 reviewed by Cedar and integrated as 2929bda4; 97 assertions, 48 refusal children, 77 dictionary controls pass. Checker follow-up uses num-parse admission while ALLDIG continues to claim numeric-shaped names. Combined engine a8846c97 passes expanded fixture and native-feed; old engine dc3b52f0 fails checker-only negatives. Await peer review of checker follow-up and final combined gate.
