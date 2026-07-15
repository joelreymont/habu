---
title: "Snapshot: rebase persisted compiler DATA"
status: active
priority: 2
issue-type: task
created-at: "2026-07-15T15:36:55.758964+02:00"
---

Full context: fresh snapshot candidates created by snap --force crash with rc 134 under both the current staged core-record load order and the exact prior load order. LLDB records pc 0x3000f0790 dereferencing the snapshot-time DATA base plus 0x7220 through orphaned compiler/startup code, falsifying the load-order change as the cause and proving a baseline snapshot restore defect. Root cause to establish: persisted compiler or startup pointers retain snapshot-time DATA addresses instead of being rebased or excluded from the image. Fix snapshot capture/restore at the owning serialization boundary; do not mask the crash or bypass fresh snapshots. Acceptance: old-order and staged-order fresh snapshot candidates both boot and load; debugger evidence proves restored pointers target the live DATA region; focused snapshot, build-fixpoint, and native fixpoint gates pass.

Claim: agent=snapshot-rebase-impl workspace=.jj-ws/habu-snapshot-rebase-persisted-4bd33351.
