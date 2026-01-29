---
title: Fix infer pass error propagation
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:04:51.871819+01:00"
---

Context: src/compiler/passes/p07_infer.zig:31-35; cause: catch returns unchanged hides type errors; fix: propagate type errors via PassError and update callers in passes/passes.zig; deps: habu-fix-bicheck-err-7bbc156d; verification: add test expecting error on bad type, run zig build test --filter infer
