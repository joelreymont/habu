---
title: Decide what a saved image holds for a pushed path
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T09:54:53.726798+01:00"
---

Problem: docs/forth.md:298-299 says an image cannot be saved with a path pushed, but no code enforces it: a top-level SOURCE-ROOT:PUSHPATH leaves a mapped copy in include.f's PUSHPATH stack (src/core/include.f:263), and an image saved then keeps a pointer to memory the restored process does not map. Acceptance: a program that pushes a path at top level and saves an image either is rejected statically or restores with an empty path stack, and forth.md states which; no runtime check. Depends: none. Claim: unassigned.
