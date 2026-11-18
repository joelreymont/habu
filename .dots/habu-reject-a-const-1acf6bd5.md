---
title: Reject a constant at the code ceiling
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T14:43:11.091917+01:00"
---

Problem: DEF-CONSTANT (src/habu/interpreter.f:2469) skips DEF-CODE-ROOM (:1528), so `constant` at the code ceiling is a kernel stop (exit 83) where `:` and `create` are rejected E-CODE-SPACE-FULL; and E-CODE-SPACE-FULL has no test since test/protection-span.f TEST-CODE (which reached the ceiling through a program's cp!) was deleted. The code area is about 30 MB (src/habu/layout.f:53, :79), a capacity a real program can reach. Acceptance: `:`, `create` and `constant` past the code ceiling are each rejected E-CODE-SPACE-FULL with the word's place; a test reaches the ceiling with real definitions and asserts all three. Depends: none. Claim: unassigned.
