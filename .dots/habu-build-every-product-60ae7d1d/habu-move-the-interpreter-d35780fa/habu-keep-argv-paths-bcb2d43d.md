---
title: Keep argv paths out of interpreted source
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T23:47:27.324213+03:00"
---

Problem: main.f loads an argv file by building source text around its path: ROW (src/habu/main.f:119-120) writes `s" <path>" provided` / `script-required` and the rows are interpreted. A path holding `"` ends the string early and the rest runs as code (a file named `x" 1 die` runs `1 die`); a path that legitimately contains a quote cannot be loaded. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix it in the code that stays, never in the machinery the parent replaces.
Acceptance: the path reaches `provided`/`script-required` as a string argument, never as source text; any path the OS accepts loads by name, and no byte of it is interpreted. A test: a file whose name holds `"` and a Forth word loads and runs its own contents only, through the real argv path.
Depends: none.
Claim: unassigned.
