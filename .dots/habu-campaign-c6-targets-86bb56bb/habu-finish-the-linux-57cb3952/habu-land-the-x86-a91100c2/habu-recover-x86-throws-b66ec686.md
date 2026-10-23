---
title: Recover x86 throws without guarded mutators
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.397075+03:00"
---

Intel integrated and pushed kwttvzymd0024147 through11a50c83. Private frame-rewind restores saved cursors, observers/maps/occurrences; public live-task guards intact. Independent adversarial review no findings; native build+gen5/gen4 identity, outer-interpret220 cases and focused native/primitive/reclaim E2Es pass. Combined profiler/debug/outer four-file run exits0. No tests changed. Pulsar workspace retired. OPEN: Mac ARM checks for habu1.f,habu2.f,interpret.f,kernel-x64.f,prims.f; final integrated full native suite.
