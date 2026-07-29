---
title: Freeze compiler IR builders
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:58.885952+02:00\""
blocks:
  - habu-store-compiler-control-e652c33a
---

Full context: design sections 6.4-6.5 require unique builders, abort, validation, and atomic freeze. Implement NEW-BUILDER, append APIs, ABORT, FREEZE result ownership, committed ceilings, and removal of mutation authority. Acceptance: every freeze arm returns context; refusal publishes nothing; abort releases all provisional storage; use-after-freeze/double-freeze/frozen mutation reject. Dependency: function/block control tables.

Claim: agent=irfreeze workspace=.jj-ws/habu-freeze-compiler-ir-6f706100
