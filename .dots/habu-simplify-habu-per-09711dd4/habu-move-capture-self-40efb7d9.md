---
title: Move capture self-tests out of aot-capture.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.060506+03:00"
---

Problem: src/habu/aot-capture.f:2842-2983 runs ACAP-NIDX-SELFTEST and ACAP-WID-SELFTEST at every load (54 files require it). Every capture also re-audits pure functions through ACAP-TIDX-PROVE (:210-230), ACAP-PROVE-RECS (:1525-1585) and ACAP-NIDX-PROVE (:305-323), called at :2792 and :2828-2829. About -265 lines. Acceptance: the prove passes deleted; the self-tests become a test file registered in the gate; capture output unchanged. Files: src/habu/aot-capture.f, a new test file, test/gate-stdlib-cases.f. Verify: aot capture tests; bin/hb --load test/run.f (capture format). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
