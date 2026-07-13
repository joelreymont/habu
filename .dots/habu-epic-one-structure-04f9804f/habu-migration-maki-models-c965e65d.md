---
title: "Migration: Maki models to unified types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:45.440216+02:00"
blocks:
  - habu-migration-libs-to-4e798110
---

Migrate maki model IR, schedules, devices, artifacts, evidence, transactions, and codecs to STRUCTURE and payload-capable ENUM with named fields. Preserve durable schema/version semantics, hashes, replay identity, PTX generation, and public APIs. Replace raw semantic kind/status/error codes with ENUM plus exhaustive external codecs where needed. Run maki/test.f, ptx-stdlib, touched native slices, replay/determinism, codec, and artifact gates.
