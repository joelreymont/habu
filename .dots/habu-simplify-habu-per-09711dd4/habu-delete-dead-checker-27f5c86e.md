---
title: Delete dead checker names and stale citations
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.881622+03:00"
---

Problem: 35 of the 45 dot ids cited in checker.f comments exist nowhere in .dots/ or .dots/archive/; 57 comment lines are plan-phase tags; CT-NONE (:1893), XFER-MASK-BITS (:3843) and ASIG-MISS-I (:9613) are dead; CHECKER-TAPE:INSTALLED-BY and BY (:22019-22042) are used only by tests. Acceptance: dead names deleted; citations of missing dots and plan-phase tags removed, keeping any fact they state; the test-only witness removed and its tests reduced to observable behavior. Files: src/core/checker.f, the tests using CHECKER-TAPE:INSTALLED-BY. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
