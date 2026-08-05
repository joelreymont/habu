---
title: Drop the stale PEINV prose
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T16:37:56.318174+02:00"
---

docs/effects.md:669 still asserts the primitive-effect axiom set is 'independently ratcheted by PEINV below' — tools/primitive-effect-inventory.f (package PEINV) was deleted by the governance mirror (df9bf14ac). Remove or rewrite the sentence (and any sibling prose in that section naming the deleted tool) to state what actually holds now: the axiom rows live in src/core/checker.f and nothing ratchets them independently. One-paragraph doc fix; found by the grooming lane.
