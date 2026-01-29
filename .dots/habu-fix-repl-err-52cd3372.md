---
title: Fix REPL error propagation
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:04:55.359245+01:00"
---

Context: src/interp/repl.zig:128-210; cause: catch {} and map all errors to InvalidArgument; fix: return/propagate exact errors and change wireGlobalEnv/setup to !void as needed; deps: habu-fix-infer-pass-ddc67df9; verification: add REPL test for error path, run zig build test --filter repl
