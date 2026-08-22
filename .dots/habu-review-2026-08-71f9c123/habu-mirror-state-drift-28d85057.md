---
title: "mirror state drift: stale trusted signature and no CP rewind"
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.246284+02:00"
---

Problem: bootstrap/cg/forth.fs:4896-4926 C-DEFER neither clears trusted state before C-PARSE-REQUIRED-SIG nor after (native habu2.f:3564, 3588 do both) and forth.fs:5743-5760 EMIT-INTERPRET-COLON has no C-CLEAR-TRUSTED-STATE (native 6128): after 'defer X ( sig )' TSIG-U stays set and the next signature-less colon takes the TRUSTED publish tail (5858-5866 -> 5817) registering it under X's signature - latent today (no sig-less colon follows a defer in the stage0 corpus, measured) and a plausible contributor to the unexplained pre-trust drain failure (dot 88a4297e); forth.fs:5838-5856 'rejected' falls through without the CP rewind native does at habu2.f:7779-7782, so every hook-rejected definition leaks its code and name bytes. Also: the mirror's C-CONSTANT (forth.fs C-CONSTANT tail) never stores LASTC-CELL where native habu2.f:3191 does, so a does> following a constant patches the PREVIOUS record (found by the seam lane 2026-08-22; publication no longer depends on it). Acceptance: both clears, the rewind and the LASTC-CELL store mirrored; a stage0 fixture with defer-then-bare-colon certifies the colon under its own (inferred) effect; a rejected-definition fixture shows CP restored. Files: bootstrap/cg/forth.fs, test/bootstrap-*-src.f. Verify: recovery gate. Depends: habu-fix-gforth-recovery-9269e3a3. Ownership: mirror. Claim: unassigned.
