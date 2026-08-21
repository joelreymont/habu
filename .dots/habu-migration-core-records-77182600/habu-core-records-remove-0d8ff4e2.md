---
title: "Core records: remove checker frames"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:59.125233+02:00"
closed-at: "2026-07-14T01:29:59.804304+02:00"
close-reason: Replaced WF/CFS/MF/RBF structure definers with exact asserted layouts at cca6cb6f; destruction review clean; focused suites, typed-local/trust/host/filemap/dot/parallel lints, no-binary recovery, full native 66842ms<=70000ms, Maki, and PTX green.
---

Own remaining checker control, MATCH, locals-width, and lowering frame layouts
in src/core/checker.f plus focused engine tests. Replace raw structure definers
with named cell/byte offsets, named strides, ordinary accessors, and load-time
offset, size, alignment, and pointer-role assertions; preserve stack-state,
snapshot, and diagnostic semantics exactly. Claim: agent=core_frames
workspace=.jj-ws/type-dsl-frames. The temporary owner-persistence blocker was
removed after destruction rejected its overlapping WIP and the clean rebuild
was confined to persistence/bootstrap files with no checker ownership.
