---
title: "Infer scheduler: conservative growth admission"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.812071+02:00"
blocks:
  - habu-infer-scheduler-req-1ac1dac6
  - habu-infer-kv-exact-a989783c
---

Why this exists:
the incremental profile needs a named conservative growth rule rather than implicit overcommit.

Required result:
define its admission headroom and recheck points using exact allocator metrics.

Done when:
deterministic traces show admit, wait, and reject decisions; requests that cannot honor their declared limit never enter decoding; cancellation returns ownership.

Expected touch points: new maki/infer/admission-growth.f, focused test.
Smallest check: focused trace and boundary tests.
Prerequisites: request state machine and exact KV metrics.
Owned result: incremental admission profile only.
Claim: unassigned.
