---
title: Measure a library edit since support inputs grew
status: open
priority: 1
issue-type: task
created-at: "2026-10-31T10:14:19.704918+02:00"
---

Found by jerry on master a9fa48b2 with its own engine: test/compiler/native-build-edit-e2e.f fails F53 since 8d02c990 'Record the cold load's reads as support inputs'. The early edit (append to src/compiler/ir/symbol.f) prints 'support image built', files reused 0 compiled 158 (no-edit repeat: support hit, reused 9 compiled 149). Logs /tmp/jerry-reuse-q3/native-build-edit-e2e-475030078158791-0/logs/. The cold load (tools/cold-load.f) loads src/habu/native-runtime.f, which requires the compiler; COLD-SOURCES:DRAIN (tools/native-build-image.f) records every file read, so native-build-support.inputs now lists the whole engine closure, src/lib/memory.f included (/tmp/jerry-reuse-m/cache/a5fdb939.../img/native-build-support.inputs). If a support rebuild invalidates every product object, the gate row's 'comment edit in src/lib/memory.f -> exactly 1 file compiled' (habu-gate-the-recompile-2ec826c7) cannot pass. Acceptance: on the lane 11 tip with its host, after P0, a comment appended to src/lib/memory.f in a tree copy: report support hit/built, reused/compiled and the first miss reasons (diag). If every file recompiles, name the key component that changes and design the fix before the gate row lands. Claim: carl (lead), runs after lane 11 P0.
