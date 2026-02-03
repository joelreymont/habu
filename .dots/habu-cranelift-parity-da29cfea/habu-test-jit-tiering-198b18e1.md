---
title: Test JIT tiering
status: active
priority: 2
issue-type: task
created-at: "\"2026-02-03T22:14:19.419958+01:00\""
blocks:
  - habu-update-parity-matrix-00bb1636
---

Context: src/interp/vm.zig:513 (enableJit hot threshold); cause: no regression test for hot-count tiering; fix: add unit test asserting compile_n stays 0 until Nth run then increments once; deps: habu-parity-tests-9be195f5; verification: zig build test.
