---
title: Split-K for the small-shape occupancy hole
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-19T14:23:36.363940+02:00\""
closed-at: "2026-07-19T20:17:11.389263+02:00"
close-reason: "Landed 5af83092 with the occupancy hypothesis FALSIFIED: split-K regresses 512-class monotonically in S (S=2 0.40x, S=4 0.34x vs 0.67x committed) - S x partial-write traffic + reduce pass outweighs occupancy gain. Ships off-by-default (byte-identical S=1), element-exact 16/16 across tf32/fp16/bf16 at 512/1024, E-CUDA-WS fail-closed workspace sizing, Round 12 recorded. 512-class parity needs a different lever"
---

Parity-plan phase 3a. At 512^3 only 32 blocks launch on 48 SMs - a third of the machine idles and no tile geometry fixes that. Split-K: partition the K dimension across 2-4 blocks per output tile (64-128 blocks total), each computing a partial C in f32, plus a deterministic reduce (two-pass: partials to a workspace buffer, then a cheap reduction kernel - NOT atomics, so element-exactness and run-to-run determinism hold; the integer-fill exactness argument extends since partial sums stay < 2^24). New launch-geometry words + the reduce kernel in lib/ptx, workspace sizing fail-closed against memory, element-exact rows for split 2/4 at 512/1024, byte-identity when split=1, then the doc protocol on spark under 13.3. Target: 512-class from 0.56-0.75x toward 0.9x+ across dtypes.
