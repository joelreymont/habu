---
title: "Infer scheduler: request state machine"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:43:30.801548+02:00\""
---

Why this exists:
scheduler states and allowed transitions are prose, so partial or illegal request progress can be represented.

Required result:
define a typed request identity and WAITING, PREFILLING, DECODING, COMPLETED, CANCELLED, FAILED, REJECTED states with explicit transition functions.

Done when:
every allowed transition succeeds once; skipped, stale, terminal, and duplicate transitions reject without mutation.

Expected touch points: new maki/infer/request-state.f, focused test, FILEMAP.md.
Smallest check: focused transition matrix.
Prerequisites: none.
Owned result: request identity and state machine only.
Claim: agent=reqstate workspace=.jj-ws/habu-infer-scheduler-req-1ac1dac6 machine=spark.
