---
title: Refresh the IR storage manifest for the scratch bump
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T19:04:19.694721+02:00\""
---

test/compiler/ir-storage-manifest.f and ir-storage-proof.f are red on the proofs branch: the byte-loop leaf raised the per-context scratch capacity 64K -> 128K (src/compiler/ir/context.f, with ir-context.f pins moved) but the storage manifest still pins the frozen 65536 literal ('the pinned capacity still carries the frozen literal / expected 65536 got 131072'). Missed at landing because per-leaf gates ran the touched suites, not the whole stdlib set. Mechanical refresh: re-derive the manifest rows from the live constants the way the file's convention prescribes, and check whether the proof file's dependent statements move with it. Master-merge blocker class (red suite on the branch).

Claim: agent=manifestlane workspace=.jj-ws/habu-refresh-the-ir-03552e37
