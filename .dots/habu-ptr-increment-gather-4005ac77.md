---
title: Pointer-increment gather lowering for idxctx
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T13:18:59.513997+02:00"
---

docs/tma-gather.md lowering 2 (docs/triton.md s7 pattern). Emit lookup-table pointer-increment gather for idxctx behind the MOVE node; portable baseline for all arches incl sm_87. Golden: gathered GEMM CPU reference vs device. After 'MOVE plan node'.
