---
title: "Infer prefill: scheduler chunk budget"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.744975+02:00"
blocks:
  - habu-infer-prefill-bounded-bd39f236
  - habu-infer-scheduler-req-1ac1dac6
---

Why this exists:
chunk size must bound decode stalls for active requests rather than be a fixed performance constant.

Required result:
define an explicit per-iteration prefill token budget consumed by the scheduler, with at most one bounded chunk before ready decodes resume.

Done when:
synthetic long-prompt traces prove the configured stall bound and deterministic progress; zero/overflow budgets reject.

Expected touch points: scheduler policy module/test only.
Smallest check: focused deterministic trace test.
Prerequisites: bounded chunk driver and scheduler request states.
Owned result: chunk budget policy only.
Claim: unassigned.
