---
title: "Help the interpreter's opener allocation by the limit"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T07:27:44.192495+01:00"
---

Problem: src/habu/interpreter.f OPENER-KEEP (:3141-3147) allocates through src/lib/memory.f:43 MEM-ALLOC-PTR, so a refusal prints E-MEM-MAP's program help (release storage the program holds) for an allocation the engine made. Acceptance: that refusal's help holds for an engine allocation; a test fails first. Depends: none. Claim: unassigned.
