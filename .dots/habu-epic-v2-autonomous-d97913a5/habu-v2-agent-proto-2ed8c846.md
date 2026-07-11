---
title: V2 agent protocol codec
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:25:27.081412+02:00"
blocks:
  - habu-v2-machine-action-a7357409
---

Add canonical request/response codecs over the action registry from MODEL-CAD-V2-PLAN.md:1588-1613 and 1939-1953. CLI and canonical JSON are projections of the same checked artifact; no raw command string is the semantic interface. Acceptance: encode/decode round-trip for every seed action, unknown action/schema/version and stale revision reject structurally, duplicate idempotency key returns the original result, and renderer golden is deterministic. Files: maki/agent/protocol.f, adapters, focused tests.
