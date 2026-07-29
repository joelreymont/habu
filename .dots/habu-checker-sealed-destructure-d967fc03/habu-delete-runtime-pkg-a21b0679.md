---
title: Delete runtime owner registry
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:48:17.806223+02:00"
blocks:
  - habu-restrict-raw-wordlist-66b28625
  - habu-reject-owner-pkg-938a7d15
---

Problem: the OWNER-WID registry duplicates compiler ownership with data rows, capacity limits, emit hooks, an AOT/snapshot trailer, validators, and production-empty machinery. Result: after raw wordlist capabilities are trusted-only and owner-construction packages cannot reopen, delete OWNER-WID only: its data rows, counters, emitter/seal file, cold/source/finalize hooks, trailer fields and validation, restore path, source-list entry, and tests whose sole subject is OWNER-WID. Keep the live PROT-WID registry, protected-memory bands, protected-publish guard, SEAL-PACKAGE behavior for compiler internals, and their tests unchanged. Add no replacement owner table, runtime check, version, migration reader, tombstone field, compatibility stub, or reserved capacity. Owner: src/habu OWNER-WID image/runtime machinery and direct wiring only. Dependencies: trusted-only raw wordlist operations and compile-time owner-package reopen rejection. Production red: habu2 emits an OWNER-WID trailer whose own production source is empty. Acceptance: exact source and generated-image inventories contain no OWNER-WID state, hook, trailer, validator, or test; old OWNER-WID trailer bytes are not parsed; PROT-WID and protected-memory gates remain byte-for-byte or intentionally offset-adjusted with identical behavior; checked hostile owner-product programs reject at compile time; engine, package, AOT, trust, native fixpoint, and exact diff gates pass. Claim: unassigned.
