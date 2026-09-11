---
title: Split HB build compose and lints
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:48.100590+02:00"
blocks:
  - habu-split-hb-build-f854f76a
---

Full context: after HB-BUILD state and CLI extraction, move the authenticated frozen-source provider adapter, source-frame diagnostic integration, lint command construction/dispatch, and direct-lint deferred hooks into reopened one-concern HB-BUILD modules. Do not create tools/hb-build-compose.f around flat text; the provider consumes the authenticated source-frame owner and keeps loader semantics exact. Use bare package-private calls internally; direct-lints.f reopens HB-BUILD and exports nothing. Acceptance: modular bad dependency JSON/text and AOT/signature lint behavior remain exact; no HBB globals; no flat composed source or diagnostic remap survives; focused source-provider/frame/hb-build/direct-lint tests green.
