---
title: Separate structure representation
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.140377+02:00\""
closed-at: "2026-04-03T14:53:00.653209+02:00"
close-reason: "done: boxed Structure runtime, generic slot/class protocol, defstruct cutover, zig build green; binary smoke still blocked by separate stdlib bootstrap bug"
blocks:
  - habu-canonicalize-pkg-state-4641836b
---

Problem: structures still alias vector or CLOS heuristics. Acceptance: generic structure representation or type-tag path is distinct and no symbol-in-slot0 heuristic remains. Files: lib/stdlib.habu:6198-6289, src/runtime/primitives/clos.zig:209-239. Verify: defstruct regressions for plain vectors versus structures and copy/type behavior. Blockers: habu-canonicalize-pkg-state-4641836b.
