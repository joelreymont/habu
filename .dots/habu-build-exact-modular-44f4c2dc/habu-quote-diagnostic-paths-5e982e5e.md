---
title: Quote diagnostic paths
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-15T23:50:06.095511+02:00\""
blocks:
  - habu-remove-synthetic-compose-373b117a
  - habu-cut-hb-build-6e53c639
  - habu-cross-check-remapped-12125855
---

Full context: SOURCE-COMPOSE:TEXT and DIAG-REMAP text output append arbitrary path bytes and include chains raw, allowing tabs and newlines to forge records. Render every path as one canonical JSON-quoted string even in text mode; update parsers to recognize only that quoted form and preserve location punctuation outside it. Reject raw or escaped composed, map, or temp identities in all JSON keys, values, and unrecognized top-level file fields before passthrough. Acceptance: LF, CR, tab, quote, backslash, colon and non-ASCII-byte paths remain one line and roundtrip; escaped top-level file leaks reject; normal paths have pinned output. Files: tools/source-compose.f/test and tools/diag-remap.f/test.

NOT READY 2026-07-16 (diagquote lane dispatched and honestly BLOCKED): the
target files tools/source-compose.f and tools/diag-remap.f do not exist on
master (05c75e94) — no SOURCE-COMPOSE/DIAG-REMAP definitions anywhere (rg
proof). They are created by habu-remove-synthetic-compose-373b117a (active,
claimed by the epic workspace) and the cut/split-hb-build chain
(habu-cut-hb-build-6e53c639 et al., open) + habu-cross-check-remapped-12125855.
blocked-by edges (frontmatter blocks:) added on THIS dot naming those creators
so dot-ready stops offering it early. Claim released (agent=diagquote, no
commits, workspace retired).
