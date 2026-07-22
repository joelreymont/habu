---
title: "Infer serve: HTTP framing"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T10:07:43.797006+02:00\""
---

Why this exists:
The HTTP boundary needs bounded, fail-closed request and response framing before OpenAI field mapping or engine calls.

Required result:
Parse the supported HTTP request line, headers, content length, and body into one bounded request frame and render status, headers, and streamed chunks from response events.

Done when:
Fragmented reads pass; conflicting or overflowing lengths, unsupported transfer modes, malformed headers, premature end, and write failure reject without publishing a request.

Expected touch points: HTTP framing module and focused byte-stream fixtures.
Smallest check: the focused fragmented and malformed framing test.
Prerequisites: none.
Owned result: bounded HTTP byte framing only.
Claim: agent=httpframe workspace=.jj-ws/habu-infer-serve-http-7f1f6959 machine=spark.
