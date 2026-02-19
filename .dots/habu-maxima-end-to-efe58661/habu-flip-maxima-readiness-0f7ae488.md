---
title: Flip maxima readiness zeros to ones
status: active
priority: 2
issue-type: task
created-at: "\"2026-02-19T09:23:12.799406+01:00\""
---

src/tests/integration.zig:5830+ currently readiness vector is {0,0,1,0,0,1,0,0,0,1,0,0} for simplifya/diff/solve/integrate/factor/limit/determinant/expand/sin/cos. Close remaining gaps generically (no maxima-only hacks) until vector becomes all ones for required operations. Depends on d0bf4864 and 547b6a50.
