---
title: Delete the never-set shadow callback and dead words
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T22:47:30.498425+01:00"
---

Problem: no word stores into NCOMP-DISPATCH:FIXED-SHADOW-CELL (src/habu/layout.f:943; a case-folded search finds only its readers, its claim src/habu/data-claims.f:119 and the x86-64 declaration src/habu/link-x64.f:682), so src/habu/habu2.f DEFER-SHADOW, (:1717, called by DEF-DEFER :1738) never calls anything, and src/compiler/native/shadow.f TAKE-CAST (:485) has no caller. Once def-cast is deleted, FRAME-CELL (layout.f:782) has no reader: habu2.f EM-ABORT-COMPILE-STATE (:2348) and src/habu/kernel-x64.f (:4971) only store 0 in it. src/compiler/native/dict.f STAMP-ORIGIN (:361), SPELL-DEFER-BINDING (:367) and SPELL-DEFER-CELL (:373) have no callers. Acceptance: these words, cells and claims are deleted with whatever only they use, and no comment names them; hb builds to a fixpoint. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-dead.
