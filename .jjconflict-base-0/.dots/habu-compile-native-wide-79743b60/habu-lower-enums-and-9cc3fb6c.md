---
title: Lower enums and match
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:39.041463+02:00"
blocks:
  - habu-lower-wide-structures-b7f12b2f
---

Full context: design Wave 6 lowers payloadless/payload `ENUM` construction and exhaustive `MATCH` with explicit tag/payload layouts and control flow. Acceptance: tag/payload/variant/coverage/layout mutations reject; wide/nested cases pass; no semantic reconstruction from bytes.
