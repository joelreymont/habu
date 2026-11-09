---
title: "Give icode.f's no-locals rule its current reason"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T21:04:26.471827+02:00"
---

Problem: src/arch/arm64/icode.f:7-8 says the file uses no locals because 'the Gforth recovery compiler checks this file before the native checker exists'; the Gforth recovery route is retired (DECISIONS.md 'recovery', 2026-11-06: no change runs a Gforth check), so that reason no longer holds (kernel-object review, 2026-11-09). Acceptance: the header states the rule that still keeps locals out of icode.f (if it still loads before the checker in a kernel build, say so), or, if none does, the rule line goes and locals are used where they read better. Files: src/arch/arm64/icode.f. Verify: a comment-only change needs none; a code change rebuilds hb byte for byte (gen 1 = gen 2) and runs it. Depends: none. Ownership: unassigned. Claim: unassigned.
