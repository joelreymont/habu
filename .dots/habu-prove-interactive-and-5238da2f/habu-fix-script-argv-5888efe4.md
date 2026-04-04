---
title: Fix script argv publish
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-04T17:13:06.560986+02:00\""
closed-at: "2026-04-04T17:25:15.160372+02:00"
close-reason: "done: removed pre-script repl.eval argv publication; CLI args are now published directly into live package globals; zig build passes; ./zig-out/bin/habu .tmp/argv-smoke.lisp a b c now reports ARGS=(a b c) instead of InvalidPackage or wrong argv"
---

Problem: src/main.zig:43-66 publishCommandLineArgs evals COMMON-LISP::*command-line-args* and SB-EXT:*posix-argv* through package-qualified reader syntax before script loading, and ./zig-out/bin/habu tools/maxima-rtest.lisp currently dies with Fatal error: InvalidPackage before the script runs. Fix the script entrypoint generically: publish argv only into packages that actually exist, preserve CL/CL-USER bindings, and make scripted execution work without package-name assumptions. Verify with zig build and a real script smoke (tools/maxima-rtest.lisp or a minimal script).
