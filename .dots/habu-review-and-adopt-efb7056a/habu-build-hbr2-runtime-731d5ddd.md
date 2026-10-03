---
title: Build HBR2 RUNTIME and UI headless in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.893214+03:00"
---

Problem: PA-r2 P7 depends on the HBR2 runtime packages (RUNTIME, UI, BROWSER; HBR2 §1.2, §27.1), which no P-package builds and which do not exist in Habu; HBR2 §27.2 lets headless component and state work run before the Wasm backend executes; Maki's viewer is the caller (maki docs/viewer.md). Acceptance: docs/browser-runtime.md holds HBR2 (revision 2, imported from Joel's design); lib/runtime/ (Id128, immutable store, RootSet/SnapshotLease, transactions, jobs, scopes) and lib/ui/ (components, bindings, reactivity, admission) compiled and tested natively under the checker; HBR2 gate G1; RUNTIME imports none of UI, SCENE, BROWSER or SYNC. Files: lib/runtime/ (new), lib/ui/ (new), docs/browser-runtime.md (new), test/browser/ (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/run.f; long jobs yield; old roots survive; a package cycle fails the build. Depends: habu-land-the-portability-dbde2246. Ownership: lib/runtime/, lib/ui/, docs/browser-runtime.md. Lane: tim; dave integrates. Claim: unassigned.
