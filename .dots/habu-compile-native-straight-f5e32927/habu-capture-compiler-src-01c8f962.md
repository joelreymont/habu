---
title: Capture compiler source tape
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:02.984231+02:00\""
blocks:
  - habu-freeze-compiler-baseline-b9777eee
  - habu-encode-compiler-ir-545ee6d1
---

Full context: design section 7.1 requires the exact checked token stream before HIR. Capture token kind, byte span, spelling slice, literal value, parser mode, and origin once; no full syntax tree or AArch64 access. Acceptance: source/check/elaboration share one tape digest; immediate words are classified intrinsic, sealed compile-time computation, or named rejection; byte/span/origin corruption rejects. Dependencies: frozen Wave 0 baseline and sealed shared substrate.

Claim: agent=srctape workspace=.jj-ws/habu-capture-compiler-src-01c8f962
