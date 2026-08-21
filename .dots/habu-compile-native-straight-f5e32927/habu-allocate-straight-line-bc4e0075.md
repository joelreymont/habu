---
title: Allocate straight-line A64IR
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:52.836871+02:00"
closed-at: "2026-07-31T17:39:45.180391+02:00"
close-reason: "Landed as c19801e794c3 on proofs: A64RA linear scan + A64RAV independent validator, reviewed hunk by hunk, suites and lints green on the exact tree, pushed"
blocks:
  - habu-idx-arm64-operands-98280863
---

Full context: design section 7.9 and Wave 2 require a deterministic no-call linear-scan allocator over verified A64IR. Compute live intervals, choose legal physical GPRs, record spills/frame slots only when required, and emit a bound allocation witness; no layout or encoding. Acceptance: overlap, reserved-register, width/class, spill-slot, and target mutations reject independently; representative straight-line code allocates deterministically.

Claim: agent=linalloc workspace=.jj-ws/habu-allocate-straight-line-bc4e0075
