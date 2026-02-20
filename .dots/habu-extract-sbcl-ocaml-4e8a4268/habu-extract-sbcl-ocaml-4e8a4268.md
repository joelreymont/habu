---
title: Extract SBCL OCaml GC techniques
status: active
priority: 2
issue-type: task
created-at: "\"2026-02-20T08:27:44.407940+01:00\""
---

docs/gc-architecture-comparison.md, src/runtime/gc.zig, src/runtime/heap.zig, bench/gc.zig: study SBCL and OCaml runtime GC designs (nursery sizing, promotion, remembered set/card scanning, major collection heuristics), map to Habu gaps with measured impact targets and implement highest-impact generic changes. Depends on habu-measure-gc-against-b81d7bc6.
