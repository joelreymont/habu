---
title: "A throw from the REPL's reader or prompt ends a tty session silently"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:27:37.473801+02:00"
---

Problem: src/habu/interpret.f's REPL loop runs the prompt's type (:435) and REPL-READ (:436, the defer at src/habu/repl.f:31) outside REPL-LINE's catch. A throw there leaves the process through the engine's uncaught path with its code and no message: in ~/.cache/tmp/repl-rca/repl-rca.f case A (a session that does `[: BAD-READ ;] is REPL-READ` with a reader that throws 9) the tty session prints ' ok', then hangs up with no '?' and no prompt, exit 9. genio's READ-LINE (src/lib/genio.f :46, :97, :98) can throw there too. Acceptance: a throw from the prompt or the reader prints the report a throw inside a line prints, then the session ends with that code (a reader that keeps failing must not loop); a throw inside a line is handled as today. Files: src/habu/interpret.f (src/habu/interpreter.f once habu-merge-the-interpreter-8de1d38d lands). Verify: the reproducer's case A prints the throw's report and exits 9; test/repl-open-definition.f rc 0. Depends: habu-merge-the-interpreter-8de1d38d (same file). Consumers: none meets it (no consumer runs a tty REPL). Ownership: src/habu/interpret.f. Claim: unassigned.
