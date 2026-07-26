---
title: Seal validated model configuration
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:50:47.348103+02:00"
blocks:
  - habu-publish-make-only-a40591e2
---

Problem: MDLCFG:mcfg exposes public UNMAKE, allowing any holder to reuse cfg-proof and cfgkey around changed fields. Required result: declare mcfg with DESTRUCT owner, replace original-block MDLCFG-MCFG:UNMAKE calls with destructure mcfg, keep BUILD as the sole proof mint, and let ;package seal MDLCFG. Remove the stale-forgery caveat. Do not change config fields, key preimage, validation, public accessors, or architecture authority. The existing MDLCFG-TEST package must remain black-box; no reopen or test friend is permitted. Owner: maki/infer/model-config.f and its existing suite only. Dependency: habu-publish-make-only-a40591e2. Acceptance: the prior external UNMAKE/re-MAKE forgery and a package-reopen variant flip from ACCEPT to checker rejection through the production package load; all original-block accessors and BUILD certify; valid/invalid config behavior and cfgkey snapshots are unchanged; no public projection exposes cfg-proof; model-config, package, typed-local, signature, and trust gates pass.
