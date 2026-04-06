---
title: Fix maxima-userdir init
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-05T23:40:15.644107+02:00\""
closed-at: "2026-04-06T11:00:12.162542+02:00"
close-reason: done
---

Problem: canonical file-driven rtest6 diverges before line 110. Manual read/meval loop over ../maxima/tests/rtest6.mac fails during early reset/init with '?ASSIGNMENT: MUST ASSIGN A STRING TO ?MAXIMA_USERDIR?; FOUND: FALSE'. Need to trace why default-userdir/maxima-parse-dirstring/maxima-getenv do not establish a string userdir under authoritative Maxima load. Files: ../maxima/src/init-cl.lisp, ../maxima/src/utils.lisp, lib/maxima-manifest.lisp, tools/maxima-rtest.lisp, src/interp/repl.zig. Acceptance: canonical tools/maxima-rtest.lisp rtest6 gets past early maxima_userdir/reset path so remaining failure, if any, is later and real. Verify: canonical rtest6 no longer emits the maxima_userdir assignment failure in traced file-driven execution.
