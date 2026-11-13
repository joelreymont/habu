---
title: Judge unsafe body calls by the bound word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:27:50.758303+02:00"
---

Problem: src/core/checker.f UNSAFE-TOK? (18877) refuses a body token E-UNSAFE by spelling, so a user word named trust, immediate, layout-buffer, newtype, sumtype, enum or product can be defined (DEF-WALL admits it) but never called from a body (hunt 633076da). Acceptance: a body call refused E-UNSAFE only when the token resolves to the engine's unsafe word; a user's same-named word in scope is called. Files: src/core/checker.f. Verify: per name, a package defining it and a body calling it under bin/hb --load rc 0; the engine's own word in a body still refused. Depends: none. Ownership: checker.f UNSAFE-TOK?. Claim: unassigned.
