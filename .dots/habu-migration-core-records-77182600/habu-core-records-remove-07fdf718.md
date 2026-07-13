---
title: "Core records: remove checker registries"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:50.404414+02:00"
---

Own checker registry record declarations in src/core/checker.f: symbol, effect,
primitive, defer, and value-record registries. Replace raw structure definers
with named cell/byte offsets, named strides, ordinary accessors, and load-time
offset, size, alignment, and pointer-role assertions while preserving arena,
snapshot, and cache ABIs. Add focused registry growth/rollback tests and no
pre-checker declaration machinery.
