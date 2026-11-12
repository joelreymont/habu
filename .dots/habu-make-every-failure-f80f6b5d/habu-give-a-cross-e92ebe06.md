---
title: "Give a cross build's top-level site its line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:12:46.100999+02:00"
---

Problem: src/core/checker.f:29474 CHECK:TOP-SITE answers no line when code compiled inside a cross (Wasm) build calls it at a file's top level, where the native build answers the line: run at master 3732ec8c, a file two requires deep, line 4 natively and none in the Wasm build, while CHECK:TOP-SITE-PATH$ and the cursor offset agree. TOK-SPELL-A is the build's own copy of the variable it reads. Every located refusal that takes its line from CHECK:TOP-SITE loses it in a cross build. Acceptance: in a Wasm build CHECK:TOP-SITE at that top level answers the same line as natively; a test case asserts it. Depends: none. Claim: unassigned.
