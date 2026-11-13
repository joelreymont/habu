---
title: "A throw from the REPL's reader or prompt ends a tty session silently"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:27:37.473801+02:00"
---

Problem: REPL (src/habu/interpreter.f:2850) runs the prompt's type and REPL-READ (the defer at src/habu/repl.f:32) outside REPL-LINE's catch (:2825). A throw there leaves the process through the engine's uncaught path with its code and no message: in ~/.cache/tmp/repl-rca/repl-rca.f case A (a session that does `[: BAD-READ ;] is REPL-READ` with a reader that throws 9) the tty session prints ' ok', then hangs up with no '?' and no prompt, exit 9. genio's READ-LINE (src/lib/genio.f:65) can throw there too. Acceptance: a throw from the prompt or the reader prints the report a throw inside a line prints, then the session ends with that code (a reader that keeps failing must not loop); case A prints the throw's report and exits 9; a throw inside a line is handled as today. Consumers: none meets it (no consumer runs a tty REPL). Depends: none. Claim: unassigned.
