---
title: "Libraries: migrate process capture records"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-24T13:08:32.817502+02:00\\\"\""
closed-at: "2026-07-24T17:43:03.142307+02:00"
close-reason: Reviewed implementation landed and verified at master@origin 4fb6f52fb815.
---

Why: lib/process.f still declares PCAP:captured and PCAP:failed with legacy PRODUCT, blocking total declaration-event release. Owner: those two declarations in lib/process.f and focused capture/result tests only. Replace both PRODUCT blocks with STRUCTURE inside package PCAP, preserving field names/order and len/rc nominal schemas, PCAP-CAPTURED:MAKE/UNMAKE and PCAP-FAILED:MAKE/UNMAKE spelling, two-cell and three-cell layouts, RESULT ok/err construction, child exit/signal behavior, captured bytes, errors, and zero-copy semantics. Update stale comments. Forbidden: SUMTYPE outcome migration, aliases, legacy parser edits, process lifecycle changes, result redesign, raw casts, unrelated cleanup. Acceptance: production process, argv, env, cwd, build-cache, and PTX toolchain capture paths retain exact ok/err effects and values; reflection/layout stable; no executable PRODUCT remains in lib/process.f; focused typed-local/package/trust gates pass. Dependency proof: master 227b5b349702 has green unified STRUCTURE and process baselines.

Claim: agent=codex-process-capture workspace=.jj-ws/habu-libs-migrate-process-61679d4e
