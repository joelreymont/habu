---
title: Give the x86-64 catch the abandon rule
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T15:57:46.627973+01:00"
---

Problem: src/habu/kernel-x64.f CATCH-RUN, (:1414) has no rule abandoning a definition opened inside the catch, which arm64 has (src/habu/habu1.f BCATCH-RUN, CATCH-PEND), and its catch frame (:1329-1340) saves no pending definition; its DEF-CLOSE-BODY (:4897) and DEF-ABORT-BODY (:4919) do not zero LOCN, CAPDEPTH, RAWNEXT, TOKLOCN, TOKDEPTH and TOKRAW, which arm64 C-CLEAR-LOCALS (habu1.f:2947-2950) clears. Acceptance: on x86-64 a catch that takes a throw abandons a definition opened since it began and clears its capture state, and test/postpone-reject.f passes there. Depends: x86-64's turn. Claim: unassigned.
