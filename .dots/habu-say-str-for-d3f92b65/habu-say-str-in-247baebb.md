---
title: Say str in test program text rows
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:47:39.710629+01:00"
---

Problem: program text and unmarked test files still define words with ^u8 n rows (tools/source-discovery-test.f 279-320, test/gate-debug-lib.f:398, gate-dictionary-lib.f:868, type-decl-suite.f 266 1457 2148, lsp-test-lib.f:5435). Acceptance: they say str, suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2text
