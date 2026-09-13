---
title: "Capture and restore the source owner's checker payload"
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T10:53:21.380871+03:00"
blocks:
  - habu-build-engine-layout-abdd0188
  - habu-preserve-complete-addr-258c0288
  - habu-keep-a-row-f2c4f3d4
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own aot-arm.f payload arm/mark/high-waters, aot-capture.f signature/type collection, checker ASIG and type-family registry mark/restore; exclude call-row recording. Payload is required for restored checked REPL. Arm source owner and freeze membership before persistence/writer tools; restore correct registry base, close on refusal. Do not retire empty sections to hide unarmed producer. Verify explicit family/signature content after cleared-buffer read, source-free checked call/wrong-type refusal and second capture, with writer-only types excluded. Consume layout leaf's distinct membership/DATA boundaries.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
