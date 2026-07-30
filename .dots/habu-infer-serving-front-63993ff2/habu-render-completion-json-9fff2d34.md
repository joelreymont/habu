---
title: Render completion JSON
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.220586+02:00"
blocks:
  - habu-infer-serve-openai-1dca13cd
  - habu-integrate-reentrant-json-34850f2f
---

Why: response serialization and its admission bound are independent from request decoding and connection storage. Interface: OPENAI-COMP:RESPONSE-BOUND ( n n -- n ) takes maximum model-name bytes and maximum output bytes and uses checked arithmetic to return the worst-case capacity across the literal completion and error envelopes, six bytes per escaped model or output byte, and maximum decimal widths. RENDER ( JSON-WRITE:writer completed -- JSON-WRITE:writer ) emits one completion response; RENDER-ERROR ( JSON-WRITE:writer code -- JSON-WRITE:writer ) emits one failed-request response. Both use only the final explicit JSON emitters, own no destination, return the writer, and expose no borrowed JSON span. The caller uses JSON-WRITE:COPY. Owner: completion and error JSON rendering plus response bound only. Production red: admission cannot prove final JSON fits and rendering still assumes ambient writer state. Acceptance: every byte class reaches the six-times bound, every decimal-width edge and arithmetic overflow is covered, usage, finish reason, and error code are exact, two writers interleave without cross-talk, actual length never exceeds RESPONSE-BOUND, and JSON-WRITE:COPY proves exact and one-short destinations atomically. Forbidden: request decode, new writer, retained serializer span, streaming, allocation, schema, version, compatibility response, metric, lint, or raw JSON fragment. Smallest owning check: bin/hb --load maki/serve/openai-render-test.f.
