---
title: Bench harness
status: open
priority: 2
issue-type: task
created-at: "2026-02-03T13:28:54.134464+01:00"
blocks:
  - habu-med-add-jit-b402aeb1
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
---

bench/: ensure real benches exist and cover VM+JIT+GC. Depends on existing perf dots: habu-med-implement-real-ff8d72f6, habu-med-add-vm-d86f80ef, habu-med-add-jit-b402aeb1. Add JSON output + regression thresholds.
