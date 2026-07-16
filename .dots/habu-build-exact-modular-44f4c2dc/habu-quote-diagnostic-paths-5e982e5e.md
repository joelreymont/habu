---
title: Quote diagnostic paths
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-15T23:50:06.095511+02:00\""
---

Full context: SOURCE-COMPOSE:TEXT and DIAG-REMAP text output append arbitrary path bytes and include chains raw, allowing tabs and newlines to forge records. Render every path as one canonical JSON-quoted string even in text mode; update parsers to recognize only that quoted form and preserve location punctuation outside it. Reject raw or escaped composed, map, or temp identities in all JSON keys, values, and unrecognized top-level file fields before passthrough. Acceptance: LF, CR, tab, quote, backslash, colon and non-ASCII-byte paths remain one line and roundtrip; escaped top-level file leaks reject; normal paths have pinned output. Files: tools/source-compose.f/test and tools/diag-remap.f/test.

Claim: agent=diagquote workspace=.jj-ws/fable-diagquote
