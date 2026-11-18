---
title: Recover a throw out of a text through its frame on arm64
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.957323+03:00"
closed-at: "2026-11-19T13:51:46.511597+01:00"
---

Problem: on arm64 a throw inside a text (EVALD > 0) does not reach the text's own catch. src/habu/habu1.f BTHROW-UNWIND sends it through EVALREC-CELL to src/habu/habu2.f EM-EVAL-THROW-RECOVER (LEVALREC), machine code that pops every escaped frame and lowers CP, NDICT and DP itself, so src/habu/interpreter.f FRAME-ROLLBACK-CODE never runs there (its FRAME-UNWOUND exit). x86-64 lands the throw in the frame's own catch (interpreter.f EVAL-FRAMED-RANGE, REPL-LINE) and recovers in Habu. Two recoveries of one thing are a split to remove (docs/design.md "Machinery being replaced"; parent habu-move-the-interpreter-d35780fa).
Acceptance: arm64 delivers a throw out of a text to the nearest handler as x86-64 does, and FRAME-RECOVER runs for every failed frame; LEVALREC, EVALREC-CELL, EM-PKG-RESYNC and PKGRESYNC-CELL are gone, and so is FRAME-UNWOUND; the parent's EM-PKG-RESYNC acceptance case is cut; the arm64 engine converges and the suites that throw out of `evaluate`, `included` and REPL lines pass.
Depends: none.
Claim: agent=jerry workspace=.jj-ws.noindex/jerry-undo.
