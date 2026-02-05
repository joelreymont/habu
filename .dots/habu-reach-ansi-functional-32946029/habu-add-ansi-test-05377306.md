---
title: Add ANSI test runner
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T22:32:05.457618+01:00\""
closed-at: "2026-02-05T22:37:08.819454+01:00"
close-reason: Added tools/ansi/run.sh and docs usage; verified logs for both runtimes.
blocks:
  - habu-pin-ansi-test-8ec33815
---

Context: /Users/joel/Work/habu/tools:1, /Users/joel/Work/habu/src/interp/repl.zig:1; cause: no repeatable runner for habu vs reference CL; fix: add /Users/joel/Work/habu/tools/ansi/run.sh with modes (sbcl|habu) and deterministic output path; deps: habu-pin-ansi-test-8ec33815; verification: command produces raw logs for both modes.
