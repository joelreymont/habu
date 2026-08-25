---
title: Fix shadow-lint stale Run line
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T19:35:27.745170+02:00"
---

Full context: the Run: comment line in tools/lint/shadow-lint.f omits lib/memory.f and lib/vector.f, so the documented standalone invocation fails while the file's own require chain works. Correct the Run: line to the actual minimal load set and verify by executing it verbatim. Trivial doc-accuracy fix; batch with the next lint-tooling lane.
