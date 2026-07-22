---
title: "Infer serve: HTTP framing"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.797006+02:00"
---

Why this exists:
The HTTP boundary needs bounded, fail-closed request and response framing before OpenAI field mapping or engine calls.

Required result:
Coordinate the syntax, bounded request, response typestate, and streaming writer
leaves. This record is not implementation work.

Done when:
all child leaves land and the integrated HTTP boundary passes fragmented,
malformed, aliasing, header-framing, and writer-failure coverage.

Expected touch points: child metadata only.
Smallest check: native dot dependency lint.
Prerequisites: child leaves.
Owned result: bounded HTTP byte-framing campaign only.
