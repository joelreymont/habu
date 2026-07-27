---
title: Bind compiler ID parity
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:50:44.124249+02:00"
blocks:
  - habu-freeze-compiler-id-2dc2c646
  - habu-prove-compiler-id-cfe76485
  - habu-prove-compiler-id-4fe3b4ea
---

Scope: fan in the completed checked ID manifest/vector artifact, completed bounded packing laws, and completed allocator laws. Bind the Rocq schema digest to the exact checked Habu digest, run every shared runtime-valid/hostile numeric vector through both executable predicates, bind the Habu checker wrong-family negative separately to Rocq type/schema distinction, and run require-replay as an executable child load-path fixture rather than a numeric predicate. Gate assumptions, unbound theorem references, and Admitted; the assumptions report must expose the reviewed atomic-CAS linearizability assumption and no hidden replacement. Acceptance: digest equality, numeric decision/diagnostic-class parity, static wrong-family parity, allocator-law binding, and require-replay pass; a schema byte, vector decision, checker rejection, replay reset, unexpected assumption, unbound theorem, or Admitted mutation fails. Full prerequisites: habu-freeze-compiler-id-2dc2c646, habu-prove-compiler-id-cfe76485, and habu-prove-compiler-id-4fe3b4ea. Ownership: parity runner/gate, replay fixture, and compiler-ID assumptions report only. Excludes shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki.
