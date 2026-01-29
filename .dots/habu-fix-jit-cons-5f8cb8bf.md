---
title: Fix JIT cons call patch
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:27.540527+01:00"
---

Context: src/ir/arm64.zig:520-531; cause: cons emits bl 0 placeholder; fix: implement relocation/patching for runtime_cons target and add unit test; deps: none; verification: run zig build test --filter arm64
