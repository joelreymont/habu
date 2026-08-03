---
title: Widen the landing gate
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T22:11:21.698712+02:00"
---

Root cause of both ratchet dots: per-landing gates run touched suites only, so committed-pin suites (stdlib-manifest, trusted-inventory, primitive-effect-inventory, text-foundation-fixtures, tool-boundary-doc-public, compiler-reloc-proof, compiler-insn-proof, storage manifests) drift silently across landings and are found late by whoever runs the full gate. This has now bitten three times (storage manifest 128K, storage manifest 256K, today's six reds). Fix structurally: a tools/landing-gate.f that runs every ratchet/pin suite (they are cheap — no timing, no compile-heavy fixtures) plus the lints, meant to run in the merge workspace on the exact tree of every landing before the bookmark moves; document it in docs/forth.md's commit gate section as a required step of the merge choreography. Until it exists the orchestrator runs the ratchet suites by hand at each landing.

Claim: agent=ratchet-repair workspace=.jj-ws/habu-reconcile-the-drifted-48eefbd9
