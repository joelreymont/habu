---
title: Help a compile-only word at top level
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T06:18:08.228723+01:00"
---

Problem: a top-level 'if', 'does>', '[char]', "[']", '[:', 'is' or '.(' gets E-UNDEFINED with help 'define `if` before this point, or check its spelling' (render.f UNDEF-HELP), from the load and check.f alike: the word is defined, it only works inside a definition, so the help names a wrong fix. Acceptance: such a word's record says it works only inside a definition and its help names the fix (move it into a ':' … ';' body), from the load and check.f alike; a test covers one of them. Depends: none. Claim: unassigned.
