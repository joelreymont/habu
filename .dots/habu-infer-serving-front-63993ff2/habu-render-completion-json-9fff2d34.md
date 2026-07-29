---
title: Render completion JSON
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.220586+02:00"
blocks:
  - habu-infer-serve-openai-1dca13cd
---

Why: response serialization and its admission bound are independent from request decoding. Interface: OPENAI-COMP:RESPONSE-BOUND uses checked arithmetic to return the exact worst-case capacity from the literal envelope, six bytes per escaped output and model byte, and fixed maximum decimal widths. RENDER takes a completed result and connection-owned destination, runs the existing JSON-WRITE serializer synchronously with no yield or nesting, and immediately copies only a complete body. It returns written(length) or short(required) with destination unchanged. Owner: completion and error JSON rendering plus response bound only. Production red: admission cannot prove final JSON fits. Acceptance: every byte class reaches the six-times bound, exact and one-short destinations behave atomically, usage and finish reason are exact, alternating connections have no cross-talk, and actual length never exceeds RESPONSE-BOUND. Forbidden: request decode, new writer, retained serializer span, streaming, allocation, schema, version, compatibility response, or raw JSON fragment. Smallest owning check: bin/hb --load maki/serve/openai-render-test.f.
