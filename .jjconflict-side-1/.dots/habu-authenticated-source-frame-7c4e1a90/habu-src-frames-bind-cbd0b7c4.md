---
title: "Source frames: bind parser bounds"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:25.320127+02:00"
blocks:
  - habu-src-frames-authenticate-e4fa236f
---

Problem: existing parsers read ambient input cells, so a child source can consume bytes beyond its authenticated extent. Acceptance: activate one authenticated frame as the sole parser source with exact cursor and limit; every parse-name, character, tick, number, string, comment, and custom parsing-immediate read is bounded by that frame. Activation validates a live generation and is mutation-free on refusal. This leaf does not implement nesting, EOF restoration, rollback, file resolution, or persistent provenance. Files: source-frame parser bridge and focused boundary fixtures. Verify: every cursor boundary, empty input, final token without newline, line and parenthesis comments, custom parse-name, stale frame, and one-byte-over probes. Depends: Source frames: authenticate owned bytes. Ownership: active-frame parser cursor and limit only. Claim: unassigned.
