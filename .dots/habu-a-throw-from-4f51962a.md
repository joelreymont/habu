---
title: "A throw from the REPL's reader or prompt ends a tty session silently"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:27:37.473801+02:00"
---

Problem: REPL (src/habu/interpreter.f) runs the prompt's type and DEBUG:REPL-READ (src/habu/stepper.f) outside REPL-LINE's catch. A throw there, such as one GENIO:READ-LINE (src/lib/genio-core.f) passes on from an input device whose accept throws, leaves the process through the engine's uncaught path with its code and no message: the tty session prints ' ok', then hangs up with no '?' and no prompt. Acceptance: a throw from the prompt or the reader prints the report a throw inside a line prints, then the session ends with that code (a reader that keeps failing must not loop); a session whose input device's accept throws 9 prints the throw's report and exits 9; a throw inside a line is handled as today. Consumers: none meets it (no consumer runs a tty REPL). Depends: none. Claim: unassigned.
