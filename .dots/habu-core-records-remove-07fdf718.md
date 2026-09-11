---
title: "Core records: remove checker registries"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:50.404414+02:00"
closed-at: "2026-07-13T23:31:08.933830+02:00"
close-reason: implemented explicit checker registry layouts; reviewed; native fixpoint, full gate, Maki, PTX, and lints green
---

Own checker registry record declarations in src/core/checker.f: symbol, effect,
primitive, defer, and value-record registries. Replace raw structure definers
with named cell/byte offsets, named strides, ordinary accessors, and load-time
offset, size, alignment, and pointer-role assertions while preserving arena,
snapshot, and cache ABIs. Add focused registry growth/rollback tests and no
pre-checker declaration machinery.

Claim: agent=checker_boot workspace=.jj-ws/type-dsl-checker
