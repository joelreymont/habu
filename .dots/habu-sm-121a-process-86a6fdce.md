---
title: sm_121a process target row + arch gates
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T13:18:59.495225+02:00"
---

docs/tma-gather.md missing-piece-4. Add GB10 target row next to sm_87: 48 SMs, 99KB smem/SM (101376B optin), bf16/fp16/fp8 yes, TMA yes (incl gather4), tcgen05 no. Arch-gate TMA words sm_90+/gather4 sm_100-family. Bench harness tools/ptx/bench.f unchanged; ptxas on spark knows sm_121a natively. Small; unblocks everything else in the doc. No dependencies.
