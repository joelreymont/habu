---
title: "Linux arm64: memory-test throws -2502 on spark"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.271172+02:00"
---

Problem: on spark with the Mac-built hb of the cross-OS stack (2026-11-11), bin/hb --load src/lib/memory-test.f prints three hex words (00000000f85f8e69, 0000000039400129, 00000000f84107fe) and ends rc 67 with 'hb: uncaught throw code -2502'. It passes on the Mac. Log: spark:~/carl-runs/xos4/o/red/memory-test.log. Cause unknown. Acceptance: the cause is named with path:line and fixed at its owner; the test passes on spark and on the Mac. Files: unknown until the cause. Verify: the test on spark. Depends: none. Ownership: carl. Claim: unassigned.
