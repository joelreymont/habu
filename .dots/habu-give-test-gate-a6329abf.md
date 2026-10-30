---
title: Give test/gate-pool.f a package
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:22:13.769006+02:00"
---

Problem: test/gate-pool.f defines its words (GT-POOL-*) as prefixed globals with no package, against CLAUDE.md 'Every module has a real package'; the timer lane 2 review (2026-10-30) counts it as pre-existing debt and adds GT-POOL-OPEN-WATCH, GT-POOL-DRAIN-STREAM and friends in the same style. Acceptance: gate-pool.f declares a package with a public section holding only what test/run.f and the gate tests call, private words lose their prefix, callers use qualified names; package and word names are Joel's (asked in the names interview with the lane 1/2 names). Files: test/gate-pool.f, test/gate-pool-test.f, test/gate-pool-orphan-test.f, test/run.f, test/gate-signal-row.f. Verify: hb --load tools/check.f on each file rc 0; hb --load test/gate-pool-test.f, test/gate-pool-orphan-test.f, test/gate-signal-test.f rc 0. Depends: habu-remove-the-test-6fc078db (lane 2 lands first). Ownership: test/gate-pool.f. Claim: unassigned.
