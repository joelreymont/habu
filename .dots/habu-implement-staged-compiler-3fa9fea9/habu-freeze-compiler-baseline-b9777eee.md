---
title: Freeze compiler baseline
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:09.564348+02:00"
blocks:
  - habu-measure-gpu-compiler-086c1a28
---

Full context: design section 14 Wave 0 requires source-pinned inventories, native and GPU measurements, a disabled capability record, and comparison-only shadow plumbing before code generation changes. Required result: close every Wave 0 deliverable, reusing existing benchmark and opt-ir evidence only for their actual scope. Acceptance: target, toolchain, source digest, protocol, production entries, unsupported capabilities, and shadow coverage are committed; no generated code or publisher changes.
