---
title: Speed capture dedupe
status: open
priority: 2
issue-type: task
created-at: "2026-02-01T22:30:21.246455+01:00"
---

Context: src/compiler/passes/p05_capture.zig:38-43; cause: O(n^2) string compare and non-interned names; fix: use Value/raw + hash set for captures; deps: none; verification: add test or zcheck for capture count
