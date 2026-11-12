---
title: Locate the engine's interpret-level refusals
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T14:31:40.370636+01:00"
---

Problem: interpret-level refusals print no place. The Habu loop's REFUSE (src/habu/interpreter.f:322-324) prints its message, the token and a newline and throws RC-REJECT; its callers (among them E-UNDERFLOW at :403, interpret-mode layout value and internal engine word at :494-502, interpret stack underdepth at :508, trusted-only tick at :797) name no file or line, though AT-SOURCE (:330-337) prints the place. In the engine, LFLOORREC (src/habu/habu2.f:3037-3042, a data-stack underflow caught by the guard page) prints its message and token and throws through LEVALREC, also with no place, while LCOMPILEDIE (:3050) appends ` at <path>:<line>` through LSRCAT (:3058). It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces. Bare library throws and the Maki case are habu-uncaught-throws-name-1a43b055's.
Acceptance: each of these refusals prints its file and line through the one located tail (AT-SOURCE in interpreter.f, LSRCAT in the engine), with its exit status and throw code unchanged; a file holding the one line `then` under bin/hb --load on a rebuilt hb prints its path and line 1 and exits 70.
Depends: none.
Claim: unassigned.
