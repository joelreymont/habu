---
title: "Infer scheduler: wait and reject reasons"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:43:30.840126+02:00"
blocks:
  - habu-infer-scheduler-strict-68f555aa
  - habu-infer-scheduler-conservative-80ce81be
  - habu-infer-scheduler-fifo-88f80a53
---

Why this exists:
users and metrics need a stable reason when a request waits or rejects.

Required result:
define typed reasons for queue capacity, strict KV, growth headroom, model limit, shutdown, and invalid request, with current/requested values where relevant.

Done when:
every decision path emits exactly one reason and rendering is deterministic; reasons do not alter policy.

Expected touch points: new maki/infer/scheduler-reason.f, focused test.
Smallest check: focused reason matrix.
Prerequisites: both admission profiles and FIFO queue.
Owned result: reason schema and rendering only.
Claim: unassigned.
