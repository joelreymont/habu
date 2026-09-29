---
title: Pass the full gate on the cross-built engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.834869+03:00"
blocks:
  - habu-inventory-the-gate-9d02ff35
  - habu-emit-x86-float-38de4a6f
  - habu-marshal-ffi-calls-46dfd999
  - habu-add-linux-family-56c75040
  - habu-pass-task-signal-bdb4c4f4
  - habu-pass-crash-and-c451eb43
  - habu-build-stripped-images-b27cfad7
  - habu-write-snapshots-and-25a9e6b7
---

Problem: the full gate has never run on an x86 engine.
Acceptance: ThinkPad `bin/hb --load test/run.f` green with the cross-built `hb-x64` at `bin/hb` in a copied tree; every red becomes a leaf under lane R, K or C; `docs/gate.md` names the x86 host and its gate command.
Files: `docs/gate.md`; fixes land as their own leaves.
Verify: ThinkPad `bin/hb --load test/run.f`.
Depends: habu-inventory-the-gate-9d02ff35 (G1), habu-emit-x86-float-38de4a6f (K13), habu-marshal-ffi-calls-46dfd999 (R2), habu-add-linux-family-56c75040 (R3), habu-pass-task-signal-bdb4c4f4 (R4), habu-pass-crash-and-c451eb43 (R5), habu-build-stripped-images-b27cfad7 (R6), habu-write-snapshots-and-25a9e6b7 (R7).
Route: Alder (shared: docs/gate.md).
Ownership: krait (Intel lane).
Claim: unassigned.
