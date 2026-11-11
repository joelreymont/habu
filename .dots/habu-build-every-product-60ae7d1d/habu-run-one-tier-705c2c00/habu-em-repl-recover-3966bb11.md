---
title: EM-REPL-RECOVER restores cells only LREAD writes
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.223839+02:00"
---

Problem: found by the JIT reach census (jit-census-notes.md item 4), inferred, not run: the one path left into habu2.f LMAIN is an uncaught throw outside REPL-LINE's catch on a tty REPL (LRREC -> LREAD -> LMAIN), where the constant, create, variable and defer rows (habu2.f :10751-10768) emit through JIT helpers with no tier check; EM-REPL-RECOVER restores cells only LREAD writes. Acceptance: a tty REPL's uncaught throw recovers into the Habu loop, and LMAIN has no caller, so the JIT deletion can remove it. Files: src/habu/habu2.f, src/habu/repl.f. Verify: a tty test that throws outside REPL-LINE's catch and then defines a constant. Depends: none; blocks the LMAIN cut of habu-delete-the-jit-d9fdaf67. Ownership: the lane. Claim: agent=jerry workspace=.jj-ws.noindex/repl-rca (root cause first, no commit; brief .jj-ws.noindex/jerry-wasm/.out/briefs/repl-recover-rca.md).

State 2026-11-11 (jerry): root cause done (lane repl-rca, no commit; reproducer ~/.cache/tmp/repl-rca/repl-rca.f on a pty). LRREC never runs under the Habu loop: every jump to it (habu1.f BTHROW :3763, habu2.f LEVALREC :12596, LDIAGRET :12720, LCOMPILEDIE :12817, LUNDERFLOW :13202) first needs LREPLROUTE (habu2.f :1256), which refuses when INP-CELL is 0 (:1263), and between lines INP-CELL is 0 (FRAME-UNLINK src/habu/interpret.f :306 and REPL-RECOVER :385 restore it; measured). Inside a line EVALD>=1 and REPL-LINE's catch is the handler, so LEVALREC hands the throw to it. A probe engine whose LMAIN entry exits 99 never exited 99. So the cells EM-REPL-RECOVER restores cannot go stale on a live path: this dot closes with the LMAIN cut (habu-delete-the-jit-d9fdaf67), which deletes LRREC and EM-REPL-RECOVER. Not run: a built application's route (src/habu/main.f :249-250, APP-RUN before the REPL); the cut lane's tests cover it. Claim: agent=jerry, closes with the LMAIN cut.
