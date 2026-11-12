---
title: Read source from stdin as the usage line says
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T16:49:56.747009+02:00"
---

Problem: the usage line (src/habu/main.f:42-47 USAGE) says `[file.f]  (source on stdin)`, but hb with no file to load stops at NO-FILES (main.f:51-52, called at :228 and :231) with `hb: no source files`, so stdin source is never read. Either stdin is not a route and the usage line is wrong, or the route is missing. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix it in the code that stays, never in the machinery the parent replaces. Acceptance: `echo "1 . cr" | bin/hb --load` and the usage line agree on a rebuilt hb: either stdin source runs, or the usage line and the error name the real requirement; a test through the real argv path holds it. Depends: none. Claim: unassigned, owner carl.
