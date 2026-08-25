---
title: Size compiler context mapping for real modules
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:15:47.680861+02:00"
---

Full context: IR-CTX:MAP-BYTES (src/compiler/ir/context.f) is a 64K creation-time constant, so ALL of a module's IR-ARENA stores together hold roughly eight thousand cells, and geometric growth abandons the smaller span each time it doubles. That is test scale, not production scale. src/compiler/ir/op.f commits real operation, value and pool ceilings as creation parameters, but a module of even a few thousand operations exhausts the shared mapping and fails with E-IR-CTX-SCRATCH — the context running out — rather than a named table ceiling, which is a confusing and wrong diagnosis. Decide between a larger mapping and a chunked scratch allocator that grows without copying; measure both against a representative module rather than guessing; and give the arena a named error for the case where the context cannot satisfy a committed ceiling, so the failure names the real cause. The operation lane deliberately did NOT change this constant because it alters the memory profile of every table sharing the mapping and belongs to a designed capacity decision.
