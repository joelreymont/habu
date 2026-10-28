---
title: Install T0 as the arm64 runtime compiler
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T21:20:39.934072+02:00"
---

Lanes A1-A6 per umbrella lane order and census jerry-t0-l1/.out/a64-census.md sec. 5. A1 shared unit step; A2 emitter rows; A3 T0:COMPILE/COMPILE-FIXED/callback/DISPATCH!/INSTALL + committed suite; A4 borrowed-dispatch gate subsets; A5 measurement vs the JIT (go/no-go: load <= 1.2x JIT insn, run <= 1.05x, outputs identical); A6 engine build carrying T0 (L7). Integration workspace .jj-ws.noindex/jerry-a64 (merge of styvunzx and ktmkppmt; rebase onto C3 when final). Stack lands whole after a Fable high feature review.
