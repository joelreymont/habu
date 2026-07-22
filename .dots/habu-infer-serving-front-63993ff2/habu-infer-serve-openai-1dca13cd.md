---
title: "Infer serve: OpenAI completions mapping"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.943179+02:00"
blocks:
  - habu-infer-serve-completion-987f5b4b
  - habu-infer-serve-engine-37b8873a
---

Why this exists:
OpenAI-compatible fields and finish reasons must map exactly onto the internal completion contract without leaking transport policy into the engine.

Required result:
Translate the supported completions request into the internal schema and translate engine events into the declared non-streaming and streaming response objects.

Done when:
Supported fields, defaults, usage counts, token chunks, finish reasons, and named errors match committed protocol fixtures; unsupported fields reject explicitly.

Expected touch points: OpenAI protocol mapping and focused JSON fixtures.
Smallest check: the focused request and streamed-response fixture test.
Prerequisites: completion request schema and engine event stream.
Owned result: OpenAI-compatible field mapping only.
Claim: unassigned.
