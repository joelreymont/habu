---
title: "Refuse a NUL in a command's arguments and environment"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:00:54.601399+02:00"
---

Problem: CMD:ARG+, CMD:ENV-ENTRY+ and CMD:ENV+'s value (src/lib/process-command.f:231-255) append the bytes raw and then a 0, and the spawn measures each with ZLEN (PREPARE, src/lib/spawn.f ROWS+), so an argument 'a<NUL>b' runs as 'a': the truncation the PROC-RAW family no longer has. Acceptance: the three words refuse an embedded NUL with E-FFI-NUL before appending anything, and their terminator is written by FFI; a test in src/lib/process-command-test.f shows each refusal. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cmdnul.
