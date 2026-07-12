---
title: "Seal owner packages: migrate CAD owners"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T15:41:55.283625+02:00"
blocks:
  - habu-seal-owner-pkgs-5688d108
---

Implement phase 4 of habu-checker-seal-owner-f7de26ff after syntax/sinks. Add final sealed-package assembly blocks for TARGET, TOOLCHAIN, fusion-region owner, artifact, evidence, and store packages after all constituent files load; move tests away from private reopen and expose only bounded public test artifacts. Do not seal monolithic MAKI until package decomposition is complete. Acceptance: hostile RAW>ID/RAW>TC/RAW>REGION/private-state reopen and qualified publish rc84; public owner APIs and generated constructor calls remain green; maki full gate, trust lints, AOT/snapshot and exact owner tests green.
