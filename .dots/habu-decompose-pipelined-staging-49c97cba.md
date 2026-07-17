---
title: Decompose pipelined staging emitter into typed steps
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T12:13:21.480276+02:00\""
---

Prerequisite for habu-checker-cp-async-6ba788a5 (blocker 2 of its 2026-07-17 analysis, see LESSONS.md): the cp.async dynamic protocol (issue -> commit_group -> wait_group -> bar.sync -> read, plus xor parity flip of %r15) is emitted ATOMICALLY inside MM-PIPE-KLOOP-WITH in lib/ptx/cg-matmul-emit.f and consumed byte-verbatim by cg-matmul.f, cg-mma.f (MMA-PIPE-KLOOP mirror), and maki/lower-mm.f - so no pipeline-protocol misuse is expressible or rejectable at the tilepipe surface. Work: decompose the fused K-loop emitters into small typed step words (stage-issue / commit / wait / barrier / read-window / parity-flip) that the future typestate capability can attach obligations to, keeping the EMITTED PTX byte-identical where consumers are pinned (capture+cmp per config, the wave-2 golden-dump method) - any config whose emission cannot stay byte-identical needs its device golden re-proven on zed (element-exact mma-gemm-check + lower-mm goldens at the 918MHz discipline). The three consumer files must move in LOCKSTEP in one commit. Constraints: exclusive ownership of lib/ptx/cg-matmul-emit.f + cg-matmul.f + cg-mma.f for the session (coordinate with any concurrent MMA perf lane - wave-3 owns cg-mma.f while active); device lane (sole zed owner) IF any emission changes, host-only if byte-identity holds everywhere; strictly typed checked words with a test per step word. Acceptance: same PTX bytes (or re-goldened device proof) for every pinned config, step words individually tested, tile-pipe-test.f + tile-pipe-neg-test.f + ptx suites green, and the cp-async dot's blocker 2 marked resolved. Files: lib/ptx/cg-matmul-emit.f, lib/ptx/cg-matmul.f, lib/ptx/cg-mma.f, tests. Ownership: ptx staging emitters. Blocks: habu-checker-cp-async-6ba788a5.

Claim: agent=decomp workspace=.jj-ws/fable-decomp (owns lib/ptx/cg-matmul-emit.f + cg-matmul.f + cg-mma.f in lockstep; sole zed owner if emission changes)
