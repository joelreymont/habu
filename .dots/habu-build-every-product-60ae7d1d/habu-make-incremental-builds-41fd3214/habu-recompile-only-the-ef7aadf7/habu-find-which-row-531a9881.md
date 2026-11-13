---
title: Find which row compiles each part after a body or string edit
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T18:31:22.639448+02:00"
closed-at: "2026-11-10T18:43:10.104304+02:00"
close-reason: measured; table in the dot and the goal dot
---

Problem: at the stack tip 2d5cdbe6 (lane 4a) a body edit of one word in src/core/util.f rewrites 146 objects and a string made longer 31; in src/habu/repl.f each rewrites 3 (six.sh run7). Which row of each rewritten part's object failed its check is not measured: an older decoder shows 15 of the string edit's 31 with no row for util.f at all (parts after a compiled part of their file, linker.f LOAD :4790). Acceptance: per edit, each rewritten part put under one cause (whole FILES row, a WORDS row, an EXTENTS row, a follower of a compiled part, unexplained) from its objects before and after; kept objects checked to list nothing that changed. Files: none changed (a measurement). Verify: the worker's table. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-d6 (brief carl-layout/.out/briefs/census-body-string-parts.md); lane 4b is briefed from its result.

Result 2026-11-10 (worker): util.f body 146 = util.f twice, 126 parts listing CORE-FOLD-C in WORDS (6 spell it, 120 through a word they ran or refer to), 18 followers; util.f string 31 = util.f twice, 9 parts listing the word [if] (its time changed: the literal's length is in its code; no part lists the string's extent), 20 followers; repl.f body and string 3 = repl.f, stepper.f, end.f (REDRAW reached from RD-LINE or INSTALL). No whole FILES row, no changed EXTENTS row, no unexplained part, no kept object listing a changed identity. Followers come from LOAD compiling every part after the first that does not link (linker.f:4790). Detail: carl-layout/.out/handoff-notes-11.md.
