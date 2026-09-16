---
title: Retire the pre-record bridge after native selfbuild
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-12T18:19:08.180438+03:00\""
closed-at: "2026-09-16T14:34:49.437253+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: The pre-record bridge is still in the build path; keep its retirement with the seed and self-rebuild work."
blocks:
  - habu-build-the-compiler-c348eab0
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own pre-record declaration in native-build.f/by-name regime in native/checker-owner.f and seed docs only. Product supplies record-capable pair; make record sole authority. Verify cold/product-hosted build, missing/incompatible-owner negatives, compiler inventory lint and provenance. No ancient stdin prerequisite, absent-field fallback or new seed architecture.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
