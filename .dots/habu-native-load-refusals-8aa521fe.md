---
title: "Native load refusals name no file:line"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T18:56:34.627874+02:00"
---

Finding: a checked body's undefined word at a native tier renders 'E-UNDEFINED habu: in x: undefined word NOPE' with no file:line; the JIT printed 'E-UNDEFINED: NOPE at file:line'. Repro /tmp/jerry-big/gaps/lrd-undef{,-jit,-t1}.f. Positions exist in --json-errors packets (token_index, byte_start). After the T0 release.
