---
title: "Infer M0 schema: run identity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:09:57.627760+02:00"
---

Why this exists:
Every benchmark sample must identify the exact code, model, pack, engine, toolchain, and target environment that produced it.

Required result:
Define and validate the immutable run-identity record with schema version, source commit, checkpoint and pack digests, engine version and flags, toolchain versions, and hardware-manifest digest.

Done when:
Canonical identity data round-trips; missing digests, unknown versions, duplicate fields, malformed tool versions, and inconsistent pack and checkpoint identity reject by name.

Expected touch points: the run-identity record and focused tests.
Smallest check: the focused identity round-trip and rejection test.
Prerequisites: none.
Owned result: benchmark producer identity only.
Claim: unassigned.
