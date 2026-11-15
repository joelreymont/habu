---
title: Delete the spelling queries nothing calls
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T18:39:10.445074+01:00"
closed-at: "2026-11-13T19:01:19.132655+01:00"
---

Problem: NDICT SPELL-CALL, SPELL-QUOT-DIN/DOUT, CATCH-CELLS (src/compiler/native/dict.f) and CTL-DEAD? (src/core/checker.f) have no caller; the checker words and the catch window record only they read stay alive. Acceptance: those words and what only they read are gone; the build and name-length, create-axiom, engine tests pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu10
