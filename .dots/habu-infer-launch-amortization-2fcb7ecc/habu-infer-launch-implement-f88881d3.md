---
title: "Infer launch: implement selected replay"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.795847+02:00"
blocks:
  - habu-infer-launch-select-695c2764
---

Why this exists:
the chosen launch mechanism must reduce overhead without hiding model state or weakening cancellation.

Required result:
implement only the frozen replay contract with versioned model, batch, workspace, and snapshot identities.

Done when:
stale identities and unsupported changes reject before replay; outputs match ordinary execution; cancellation and cleanup release every capture owner; inter-token latency improves.

Expected touch points: new replay module/test and minimal engine integration.
Smallest check: correctness parity plus M0 before/after benchmark.
Prerequisites: select replay mechanism.
Owned result: selected replay implementation only.
Claim: unassigned.
