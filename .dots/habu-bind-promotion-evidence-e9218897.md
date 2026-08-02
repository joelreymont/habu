---
title: Bind promotion evidence to executed gates
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:00:40.911730+02:00"
---

Current maki/typestate.f and maki/evidence/schema.f can mint build, certify, golden, gradcheck, and profile witnesses after dropping caller inputs; the private tokens prevent direct type substitution but do not prove the represented work ran. Replace these skeleton transitions with package-private gate implementations whose only success result is immutable evidence bound to the exact artifact bytes, target, toolchain, configuration, numeric policy, test population, verifier version, result, and environment. ART:BUILD must be produced by the real builder, CERTIFY by the real checker, GOLDEN and GRADCHECK by actual comparisons, and PROFILE by the measured command; callers may not supply achieved provenance as authority. Promotion must validate complete evidence applicability, identity, freshness, and policy closure. Remove or privatize every public transition with no executable owner. Add fabrication, wrong-artifact, stale-toolchain, policy-swap, result-mutation, skipped-command, and replay tests, plus one positive end-to-end promotion that runs each gate. Reuse the landed V2 applicability and authority schemas rather than inventing another proof vocabulary. Files: current typestate/evidence gate owners and adapters; exact command runners remain one-concern modules. Verify focused evidence/promotion tests, Maki suite, artifact/store replay, device evidence where applicable, package/host/dot lints, and full native gate.
