---
title: "Enhance load with :external-format"
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:57.090906+02:00"
---

src/runtime/primitives/io.zig: Add encoding support to load
- Depends on: dot (compile-file primitive)
- load: support both .habu and .hfasl files
- Add :external-format keyword (default :utf-8)
- Handle character encoding when loading source
- Prefer .hfasl if newer than .habu
- Add tests for encoded file loading
- Est: 25 min
