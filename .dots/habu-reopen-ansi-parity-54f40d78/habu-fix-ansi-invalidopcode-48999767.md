---
title: Fix ANSI InvalidOpcode regression
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-06T03:54:40.070637+01:00\""
closed-at: "2026-02-06T04:04:04.768981+01:00"
close-reason: Removed InvalidOpcode regression by fixing local-slot bounds and max-local analysis for progv/handler paths
---

Context: /Users/joel/Work/habu/docs/ansi/raw/habu-latest.log and /Users/joel/Work/habu/src/interp/vm.zig opcode dispatch; cause: latest habu ansi run fails with InvalidOpcode (unexpected vs baseline UnboundVariable); fix: reproduce minimal failing form from latest log, trace emitted opcode/dispatch mismatch, patch emitter/vm/opcodes for consistency, add regression test; deps: habu-check-regression-and-f81466fe; verification: rerun habu latest and regression report has no unexpected_failures.
