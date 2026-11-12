---
title: Tail-call primitives in T0 as the JIT does
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T09:56:38.315347+02:00"
---

The JIT ended a body calling a primitive with a plain b; T0 tail-calls only words in the code region (src/habu/t0.f:1135) and keeps the frame for a primitive. Needs the publisher to relocate a b into engine code.
