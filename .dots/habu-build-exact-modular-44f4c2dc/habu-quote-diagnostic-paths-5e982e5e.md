---
title: Quote diagnostic paths
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:06.095511+02:00"
blocks:
  - habu-stable-source-origin-frame-9d4b2a61
---

Re-scoped 2026-07-21 after the authenticated-source destruction review: flat SOURCE-COMPOSE text, SOURCE-MAP, and DIAG-REMAP are being deleted. The remaining invariant belongs at the direct diagnostic renderer over STABLE-SOURCE-ORIGIN records. Render every logical source identity and include-chain member as one canonical JSON-quoted string in both text and JSON modes; location punctuation stays outside the quoted value. Raw filesystem, checkout-root, temporary, escaped intermediary, and mutable path identities must never pass through as logical source identity. Acceptance: LF, CR, tab, quote, backslash, colon, invalid UTF-8 bytes allowed by the path contract, and non-ASCII paths remain one record and round-trip; injected raw/temp identities reject; normal paths and nested frame chains have pinned text and JSON output. Files: stable source-origin diagnostic renderer and focused tests only. No source composition, source map, or remap modules may be recreated.
