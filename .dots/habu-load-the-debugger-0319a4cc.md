---
title: Load the debugger at first use from the REPL
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T21:42:34.720481+03:00"
---

Problem: the manifest lane (51561aeb) removed the debugger trio from the engine and `require src/habu/debug.f` loads it (about 14 ms), but dot habu-bake-only-what-4039004a's acceptance (3) asked for the debugger to load at FIRST USE from its REPL command; an unknown-word hook that loads a file is a new REPL mechanism the lane did not invent. Acceptance: a REPL session that types a debugger word (BP+, BP*, BP-, BP., step) without having required src/habu/debug.f gets it loaded once and the command runs, through a declared REPL facility (a command table naming the file that provides each command, or a missing-word hook bounded to a declared list), not a general autoload; a program compiled with tools/hb-build.f is unaffected; test/engine-stack-debugger.f and test/debugger-resume.f gain a case that does not require the file first. Files: src/habu/repl.f, src/habu/debug.f, test/. Verify: the two debugger suites; test/run.f. Depends: none (51561aeb landed). Ownership: REPL. Claim: unassigned.
