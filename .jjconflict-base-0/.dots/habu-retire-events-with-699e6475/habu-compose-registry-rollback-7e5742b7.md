---
title: Compose registry rollback owners
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T07:47:37.189281+02:00"
blocks:
  - habu-reserve-registry-rollback-a9d53ed5
  - habu-own-type-registry-e8f77b18
  - habu-own-declaration-event-643f3573
---

Problem: checker core, type/schema registries, and declaration events need one atomic rollback order without duplicating state or leaving multiple hook installers. Owner: new sealed package CHECKER-REGISTRY-SCOPE and the sole REG-EXT-RB hook installation only. Install RESERVE in TYPE then EVENT order; SAVE in TYPE then EVENT order; RESTORE-READY, RESTORE, FINALIZE-READY, and FINALIZE in EVENT then TYPE order. The event readiness check runs while fields are still live, and every readiness phase completes before any mutation. Remove the old type-family hook installer. Do not add a generic callback registry, raw forwarding global, verifier-specific cleanup, or TRUSTED boundary. Acceptance: production checker scopes call each owner exactly once in the frozen order; mutations omitting either owner, reversing restore order, duplicating installation, or mutating before all readiness checks fail. Nested scope depth remains lockstep. Files: new src/core/checker-registry-scope.f and one focused composition test; no loader manifests in this leaf. Smallest check: direct require of the module followed by real CHECKER-SCOPE-START/DONE/FINALIZE over type and event rows; typed-local and package gates.
