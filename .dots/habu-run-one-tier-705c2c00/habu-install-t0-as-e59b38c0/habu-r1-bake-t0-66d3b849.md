---
title: "R1: bake T0 into the arm64 engine, tier 0 still the JIT"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T17:03:07.413065+02:00"
closed-at: "2026-10-31T12:12:38.628254+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge"
---

Workspace .jj-ws.noindex/jerry-r1 (from master e49421f0). Brief /tmp/jerry-big/t0-r1-bake-brief.md: native-runtime.f requires t0-arm64.f and t0.f after compiler.f on arm64 targets (placeholder word LOAD-T0); build-producer.f COMPILER-DONE freezes the emitting-compiler role at t0.f on arm64, compiler.f on x86-64. No select, no install, no dispatch change. Proofs: native-build candidate, tier.f, session.f, t0-install.f, t0-child.f session.f against the public-hb baseline /tmp/jerry-big/r1-base (all rc 0), manifest-lint, check.f. Landing: full suite, two-generation convergence and the engine edit matrix (role freeze point moved).
