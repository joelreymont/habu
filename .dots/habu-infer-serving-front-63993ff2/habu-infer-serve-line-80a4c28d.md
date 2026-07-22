---
title: "Infer serve: line protocol"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.503525+02:00"
blocks:
  - habu-infer-serve-engine-37b8873a
---

Why this exists:
The first local interface needs deterministic request framing and streamed output over standard input and output.

Required result:
Map one JSON line request to the completion schema and render each engine event as one JSON line with explicit request identity and terminal status.

Done when:
Fragmented input, multiple requests, malformed JSON, invalid requests, streamed tokens, named errors, end of input, and output failure have deterministic results without corrupting another request.

Expected touch points: the standard-input and standard-output front end and focused process tests.
Smallest check: the focused line-protocol process test.
Prerequisites: engine event stream.
Owned result: JSON-line parsing and rendering only.
Claim: unassigned.
