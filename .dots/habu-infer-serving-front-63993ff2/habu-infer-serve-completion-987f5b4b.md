---
title: "Infer serve: completion request schema"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.206651+02:00"
blocks:
  - habu-infer-pack-boring-c8e07d29
---

Why this exists:
Line and HTTP transports need one transport-independent request contract before either front end can call the engine.

Required result:
Define validated completion requests with model-pack identity, prompt text or tokens, output limit, sampling parameters, stop identifiers, and request identity.

Done when:
Canonical requests round-trip; conflicting prompt forms, wrong pack identity, invalid limits, unsupported sampling, duplicate fields, and malformed text reject before admission.

Expected touch points: serving request schema and focused tests.
Smallest check: the focused request round-trip and rejection test.
Prerequisites: model-pack runtime loader and sampling parameter contract.
Owned result: completion request data and validation only.
Claim: unassigned.
