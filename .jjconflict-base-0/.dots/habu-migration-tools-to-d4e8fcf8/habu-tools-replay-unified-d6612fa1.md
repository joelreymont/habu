---
title: "Tools: replay unified declarations"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:18:11.919659+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own src/habu/verify-source.f declaration replay and focused verification tests. Recognize only STRUCTURE and full/compact ENUM as live definers, replay transactional metadata exactly, and reject removed/mixed tokens with precise diagnostics. Keep removed tokens reserved, never silently skipped.
