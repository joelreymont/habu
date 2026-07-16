---
title: Typed pipelined register-blocked GEMM tile vocabulary
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T18:57:13.550143+02:00\""
---

Capability dot minted from the mmreexpr BLOCKED report (habu-re-express-tiled-9cc4a73a): the landed tile words (TILE-LOOP + tile-smem STAGE/SLOAD + tile-acc) lower to the NAIVE per-lane synchronous scalar GEMM (~3.6x slower); lib/ptx/cg-matmul.f EMIT-MATMUL emits a register-blocked cp.async double-buffered vectorized kernel (golden: 921 lines, 12 cp.async.cg.shared.global + commit/wait_group, 32 ld.shared.v4.f32, 128 scalar ld.shared, 512 fma = 32 k-steps x 16 register-blocked FMAs, KLOOP/KEND/NOPF/PFDONE pipeline labels, 16 accumulators). Missing typed capabilities, all four needed to byte-lower the exact kernel: (1) cp.async multi-stage double-buffered shared staging word family (cp.async.cg.shared.global + commit_group/wait_group + buffer parity; landed STAGE is sync single-element ld.global->st.shared->bar.sync); (2) register-blocked RxC micro-tile accumulator with operand reuse (16 FMAs per k-step from 4 A + 4 B loads; landed acc is one scalar + one FMA); (3) vectorized shared load ld.shared.v4.f32 (landed SLOAD scalar; tile-v4a vec4 is GLOBAL-only); (4) blocked 2-D shared layout As[64][32]+Bs[32][64] strided-A/contiguous-B align 16 (landed shared-span is flat 1-D per-lane). Discipline: the tile-v4a byte-identity method - new nominal words whose bodies REUSE the existing MM-* emitters verbatim so the typed kernel lowers byte-for-byte, THEN the checked KERNEL: body replaces the bespoke MM-STATE/MM-A/B/C-REG boundary and its TRUSTED.md rows (630-633, 869-871, inventory 1306-1309). Constraint: MM-THREAD-SETUP/MM-ACC-ZERO-EMIT/MM-PIPE-KLOOP(-WITH)/MM-KSTEP-TILE/MM-TM are consumed VERBATIM by maki/lower-mm.f LMM-BLK-*/LMM-MMA-BODY and lib/ptx/cg-mma.f (byte-sensitive, device-golden-pinned) - the vocabulary must not perturb them. Acceptance: typed composition emits byte-identical PTX to the current EMIT-MATMUL golden (capture+cmp); certifies; gemm-checked tests + ptx suites green; gemm-bench capture unchanged; lower-mm/cg-mma consumers byte-identical; device golden (tools/ptx/device-gold.f GEMM-GOLDEN - NOT the dot-spec's stale matmul-device-test.f) stays wired, pending zed. Files: lib/ptx/tile-smem.f or new tile-pipe.f, tile-acc extension, lib/ptx/cg-matmul.f, tests. Ownership: kernel type system. Unblocks habu-re-express-tiled-9cc4a73a.

Claim: agent=mmreexpr workspace=.jj-ws/fable-tilepipe (lane continues from the re-express BLOCKED report; capability lands first, re-express re-opens behind it)

LONG-TERM PROGRAM (orchestrator, 2026-07-16, user directive: no hacks, best
long-term approach): this dot is stage 1 of a tracked three-stage program with
ZERO permanent trust as the end state.
- Stage 1 (this dot): typed vocabulary whose nominal families encode every
  invariant the CURRENT type system can state fail-closed - buffer parity as
  distinct nominal types (not comments), the 16B/vec4 alignment obligation on
  the shared-load word itself, 2-D layout strides as type/extent parameters,
  pipelined-tile nominally distinct from plain tile so every scalar/naive path
  rejects at the type level. Bodies reuse the proven MM-* emitters verbatim
  (byte-identity is the only honest correctness bridge while zed is down).
  Every body the checker cannot yet certify is a NAMED TRUSTED boundary whose
  TRUSTED.md owner is habu-checker-cp-async-6ba788a5 (a live capability dot -
  NOT a cap: permanent owner): these rows are scheduled for deletion, not
  parked.
- Stage 2 (habu-checker-cp-async-6ba788a5): the checker learns the cp.async
  pipeline typestate (pending/ready parity tokens, consume-exactly-once,
  read-requires-wait, loop-carried parity; composes with the M5 barrier model
  eb0716f1); the stage-1 TRUSTED bodies are re-expressed as checked code,
  byte-identity preserved, rows REMOVED.
- Stage 3 (habu-re-express-tiled-9cc4a73a): KERNEL: MM re-expressed from the
  typed words, bespoke MM-STATE boundary + its rows deleted, device golden
  re-proven on zed's return.
