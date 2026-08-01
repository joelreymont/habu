---
title: Delete runtime owner registry
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T20:48:17.806223+02:00"
---

Problem: after its fixtures and persistence path are gone, the production-empty OWNER-WID in-memory registry still duplicates compiler ownership through data rows, counters, capacity, primitives, checker effects, protection hooks, trust rows, and inventories. Result: delete that remaining runtime surface and its direct initialization/finalization wiring. Keep PROT-WID, protected-memory bands, protected publication, SEAL-PACKAGE behavior, and their tests unchanged. Owner: OWNER-WID runtime state, primitives, checker model, and direct trust/inventory rows only. Production red: current source still allocates and exposes an OWNER-WID registry whose live count is always zero. Acceptance: exact source, checker-effect, trust, test, and generated-engine inventories contain no OWNER-WID symbol or reserved runtime state; PROT-WID behavior is unchanged; engine, package, AOT, snapshot, native fixpoint, and exact diff gates pass. Forbidden: replacement table or marker, version, reader, tombstone, compatibility stub, reserved capacity, unrelated package change, or lint. Smallest owning check: rebuild the native engine, prove the complete OWNER-WID inventory empty, and run the existing protected-WID hostile suite. Claim: agent=codex-owner-runtime workspace=.jj-ws/habu-delete-runtime-pkg-a21b0679.
