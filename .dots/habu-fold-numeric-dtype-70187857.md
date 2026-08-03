---
title: Fold numeric dtype constants
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T16:31:35.358100+02:00"
---

Why: MAKI publishes five numeric DT-* constants (maki/tensor.f:122-123) whose only production consumer is DTYPE>N's own body; every other reference is a test expectation, so five public names exist to spell literals. Result: delete public DT-F32/DT-F16/DT-BF16/DT-U32/DT-I32, inline the ordinals as literals in DTYPE>N (the wire-code authority sched-key.f folds through), and migrate the test expectations in maki/tensor-test.f, tensor-value-test.f, model-ir-test.f, and examples/nanogpt/from-scratch-model-test.f to literal values. DTYPE>N and LAYOUT>N stay: sched-key.f:140-141 is their live consumer. Owner: package MAKI in maki/tensor.f plus the named test callers only. Production red: XREF finds no non-test consumer of any DT-* constant outside DTYPE>N. Acceptance: the five names do not resolve; DTYPE>N output unchanged (wire codes byte-identical, sched-key key parity on a fixture); focused tensor/tensor-value/model-ir suites, typed-local, and package gates pass. Forbidden: renaming DTYPE>N, a replacement enum, compatibility constants, or unrelated tensor cleanup.
