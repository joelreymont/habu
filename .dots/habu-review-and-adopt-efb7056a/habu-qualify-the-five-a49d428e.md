---
title: Qualify the five compiler-host products
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:29.024631+03:00"
---

Problem: no bootstrap graph, B0/B1/B2 receipts or content-versus-provenance split exists; the Intel recovery route is a cross-build from ARM64 (INTEL.md) (PA-r2 §22, P11). Acceptance: the bootstrap graph recorded; a B1/B2 canonical comparison on each brought-up host; source-free startup; the qualification matrix (§24.5) with required versus passed. Files: tools/build-fixpoint.f, tools/native-emit.f, profiles/ (new). Verify: engine fixpoint per host; receipts attached. Depends: habu-review-and-schedule-dbdba551, habu-link-native-fragments-5949ec30, habu-own-host-svcs-b1368bbe. Ownership: tools/build-fixpoint.f, profiles/. Lane: dave. Claim: unassigned.
