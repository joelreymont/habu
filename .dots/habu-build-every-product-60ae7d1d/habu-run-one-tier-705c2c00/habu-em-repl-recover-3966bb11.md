---
title: EM-REPL-RECOVER restores cells only LREAD writes
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.223839+02:00"
closed-at: "2026-11-13T19:15:48.595985+01:00"
---

Problem: found by the JIT reach census, inferred, not run: the one path left into habu2.f LMAIN was an uncaught throw outside REPL-LINE's catch on a tty REPL (LRREC -> LREAD -> LMAIN), where the constant, create, variable and defer rows emitted through JIT helpers with no tier check; EM-REPL-RECOVER restored cells only LREAD writes. Root cause (lane repl-rca, no commit; reproducer ~/.cache/tmp/repl-rca/repl-rca.f on a pty; brief .jj-ws.noindex/jerry-wasm/.out/briefs/repl-recover-rca.md): LRREC never ran under the Habu loop, since every jump to it first needed LREPLROUTE, which refused when INP-CELL was 0, and between lines INP-CELL is 0. Acceptance: a tty REPL's uncaught throw recovers into the Habu loop, and LMAIN has no caller, so the JIT deletion can remove it.

State: met. LMAIN, LRREC, LREPLROUTE and EM-REPL-RECOVER have no definition in src (rg); the JIT's deletion (habu-delete-the-jit-d9fdaf67) is closed. Between lines the Habu loop restores the input through REPL-RECOVER (src/habu/interpreter.f:2788) and FRAME-UNLINK (:2695). Claim: agent=jerry, closes with the LMAIN cut.
