---
title: Split HB build compose and lints
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:48.100590+02:00"
blocks:
  - habu-split-hb-build-f854f76a
---

Full context: after HB-BUILD state and CLI extraction, move source materialization, authenticated diagnostic remap, lint command construction/dispatch, and direct-lint deferred hooks into reopened tools/hb-build-compose.f and tools/hb-build-lints.f. Use bare package-private calls internally; direct-lints.f reopens HB-BUILD and exports nothing. Acceptance: modular bad dependency JSON/text and AOT/signature lint behavior byte-identical; no HBB globals; focused composer/remap/hb-build/direct-lint tests green.
