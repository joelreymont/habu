---
title: Delete dead checker names and stale citations
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.881622+03:00"
closed-at: "2026-11-14T12:11:20.129694+01:00"
close-reason: CT-NONE deleted; no dot citations or phase tags remain
---

Problem: 35 of the 45 dot ids cited in checker.f comments exist nowhere in .dots/ or .dots/archive/; 57 comment lines are plan-phase tags; CT-NONE (:2533) is dead. Acceptance: dead names deleted; citations of missing dots and plan-phase tags removed, keeping any fact they state. Files: src/core/checker.f. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
