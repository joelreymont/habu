---
title: Half-precision ldmatrix fragment feed
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T14:23:36.358041+02:00"
---

Parity-plan phase 2 (after the BN-wide tile lands). The half-precision B fragment is still built from scalar loads (k-major: two u16 + shift/or per register; transposed-Bs: one b32 per register but a scalar-transpose staging cost that loses small shapes). ldmatrix.m8n8.x4.b16 is DESIGNED for half fragments: one instruction fills four b32 fragment registers straight from shared memory in the mma-native layout, for A and B both. Wire ldmatrix fragment loads for fp16/bf16 (the tf32 A-ldmatrix path at MMA-LMODE is the in-house precedent; the fragment layouts differ - derive from the PTX ISA half-fragment spec), retire E-MMA-DTYPE's ldmatrix rejection for halves, and sweep BK=64 at half precision (more K per stage; the smem budget allows it at half element size). Element-exact zero-tolerance first (same integer-fill argument), byte-identity for every existing config, fail-closed on remaining unwired combos, then the doc protocol on spark under the pinned 13.3 ptxas. Target per the parity plan: fp16/bf16 mid/large shapes from ~0.5x toward 0.8-1.0x Triton same-dtype.
