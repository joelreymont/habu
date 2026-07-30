---
title: Allocate straight-line A64IR
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:52.836871+02:00\""
blocks:
  - habu-idx-arm64-operands-98280863
---

Full context: design section 7.9 and Wave 2 require a deterministic no-call linear-scan allocator over verified A64IR. Compute live intervals, choose legal physical GPRs, record spills/frame slots only when required, and emit a bound allocation witness; no layout or encoding. Acceptance: overlap, reserved-register, width/class, spill-slot, and target mutations reject independently; representative straight-line code allocates deterministically.

Claim: agent=linalloc workspace=.jj-ws/habu-allocate-straight-line-bc4e0075
