---
title: Add structural regression for literal-root collector coverage
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-08T17:08:50.327982+01:00\""
closed-at: "2026-04-02T16:40:51.153958+02:00"
close-reason: done; zig build ok, zig build test unchanged 5-error baseline
blocks:
  - habu-sync-supported-node-67f74e24
---

Files: src/testing/compile_chunk.zig, src/interp/repl.zig, src/jit/backend.zig. What: add regression(s) that fail loudly if collector coverage drifts from the supported backend surface again; prefer shared helper or explicit structural test. Why: prevent silent else=>{} omissions when future JIT IR nodes are added. Verification: focused test run in existing Zig test target or compile_chunk tests proving mismatches are caught.
