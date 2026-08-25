---
title: V2 checked async pipeline
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.059595+02:00"
closed-at: "2026-07-18T09:09:09.217362+02:00"
close-reason: "Acceptance discharged: missing-wait/barrier-phase/lifetime-escape pinned by landed cpp-slot-neg + cg-mma-slot-neg + tile-pipe-neg N7 fixtures; target/depth legality delta landed as CPLEGAL (maki/cp-async-legal.f, E-CP-ASYNC-TGT fail-closed, suite green); emitted cp.async unchanged (byte-pinned goldens, device-proven history). Follow-ons folded into habu-feed-mma-config-d783e33b."
---

Problem: MODEL-CAD-V2-PLAN.md:1397-1417 requires cp.async multistage lowering, but the checker cannot yet prove shared-tile lifetime, barrier phase, or pipeline-depth legality. Fix: implement the first typed two-stage global-to-shared pipeline capability and lower one GEMM tile through it. Acceptance: missing wait, wrong barrier phase, shared lifetime escape, and unsupported target reject statically; emitted cp.async sequence passes ptxas and device golden. Files: src/core/checker.f, lib/ptx/tile.f, lib/ptx/cg-matmul.f, test/type-ptx*.f. Verify: negative checker fixtures, ptx-stdlib gate, Orin golden.

Claim: agent=casync workspace=.jj-ws/fable-casync (owns new maki async-execution files + tests)

Acceptance status (casync survey, 2026-07-18): most acceptance surface was already
discharged by the landed cp.async program (dots habu-checker-cp-async-6ba788a5 +
habu-wire-cppslot-typestate-ce2463df); only the target-legality leg was a genuine
delta. Each item mapped to its fail-closed fixture (all green on this tree):

- missing wait -> LANDED. cpp-slot-neg-test.f N2 (WAIT of a cpp-pending rejects) +
  cg-mma-slot-neg-test.f BAD-DW (dropped wait/sync fence: READ of a cpp-committed
  rejects, needs cpp-ready), on the production MMA-STAGE-ISSUE mint.
- wrong barrier phase -> LANDED. cpp-slot-neg-test.f N3 (double-wait) + N5
  (BAD-DIVBAR: WAIT bar.sync fence under divergent control, E-DIVERGENT-BARRIER) +
  cg-mma-slot-neg-test.f BAD-WBC (wait-before-commit); the committed->ready barrier
  composes with M5/M5b (docs/type-families.md 9.1.2).
- shared lifetime escape -> LANDED (mechanism), acceptance now named. The staged
  shared tile (mmstage) is minted per-iteration inside PIPE-LOOP; the body effect
  [ mmstage mmracc -- mmracc ] forbids it in the output, so it cannot escape its
  pipeline scope: tile-pipe-neg-test.f N7 (annotated as the lifetime-escape pin),
  bounded below by READ-STAGE (needs a block-visible cpp-ready<p> slot) and by
  span-space distinctness (tile-pipe-neg N3/N6). General shared-memory lifetime /
  borrow is a distinct capability (habu-add-bounded-host-b40b048f), not this dot.
- unsupported target / pipeline-depth legality -> DELTA LANDED. New checked gate
  maki/cp-async-legal.f (package CPLEGAL): REQUIRE ( bufb stages target-id -- )
  consults the REAL target descriptor (TARGET:CAP-ASYNC, TARGET:CAP-BARRIER,
  SHARED@) and rejects fail-closed (E-CP-ASYNC-TGT) a target lacking the async
  engine / block barrier, or a depth whose staged buffers overflow the target's
  shared budget. This is the honest depth-vs-target LEGALITY check, NOT an
  emit-time per-slot typestate (depth>1 emit-time typestate is permanently refuted,
  habu-wire-cppslot-typestate-ce2463df). Fixtures: maki/cp-async-legal-test.f
  (raw-descriptor pos/neg + interned-SM87 id path). Two small public raw-descriptor
  accessors added to maki/target/target.f (DESC-SHARED@ / DESC-CAPS@) so probes
  never touch the capped target registry.
- emitted cp.async passes ptxas + device golden -> LANDED off-device by
  byte-identity. The single-buffer path (MMA-PIPE-KLOOP-SINGLE threaded through
  CPPSLOT) and the double-buffer path are the SHIPPING lowered GEMM, device-proven
  element-exact (mma-gemm-check history) and byte-pinned against the EMIT-MATMUL
  golden (lib/ptx/tile-pipe-test.f, cpp-pipe-step-test.f). CPLEGAL is a pre-lowering
  gate that emits nothing on the supported sm_87 target, so the emitted sequence is
  unchanged - no new device run needed.

Remaining (dotted follow-ons, out of this delta): wire CPLEGAL:REQUIRE into the
cad-6 autotuner's per-target candidate legality once stage selection actually flows
to lowering (today the depth is a fixed global config, so invasive selection wiring
would be speculative multi-target plumbing); and add a focused regression for the
existing lib-local MMA-CHECK-SMEM / E-MMA-SMEM guard (currently untested).
