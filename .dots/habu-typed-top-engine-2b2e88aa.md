---
title: "typed-top: engine top-row hook cell + sealed install + dispatch events"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T10:38:59.756805+02:00"
---

Sub-dot 2 of docs/typed-top-level.md sec 5 (landed 8cefda08). Files: src/habu/habu1.f (DATA cell, sealed install mirroring BSETCHECK), src/habu/habu2.f (EM-INTERPRET-FIND/-NUMBER/string-keyword/C-TICK/C-CHAR event emission), fixture test/top-row-hook-test.f (logging hook). Acceptance: hook uninstalled = current gate byte-green; logging hook observes correct (class, token, flags) per literal/word/tick class; invalid install fails closed like BSETCHECK. Independent of sub-dot 1.
