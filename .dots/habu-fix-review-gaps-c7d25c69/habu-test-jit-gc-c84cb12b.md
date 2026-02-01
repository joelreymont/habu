---
title: Test JIT GC roots
status: open
priority: 2
issue-type: task
created-at: "2026-02-01T22:30:13.091044+01:00"
blocks:
  - habu-wire-jit-gc-ae1f8f22
---

Context: src/jit/jit.zig tests; cause: no GC coverage for JIT stack; fix: add JIT test that forces GC with live stack values; deps: habu-wire-jit-gc-ae1f8f22; verification: zig build test
