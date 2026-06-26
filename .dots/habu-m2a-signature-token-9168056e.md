---
title: "M2a: signature token lexer for parametric atoms"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:03:30.747061+02:00"
---

Part of PTX M2 (epic habu-ptx-m2-parametric-a854a419). Lex the parametric signature tokens in stack comments: the < > , delimiters plus the prefixed atom namespaces space-* extent-* mask-* block-* align- (ptx-sketch.md Types + Resolved-M1/M2 #5; single letters stay type/row vars).
- Files: the atom/token parser in src/core/checker.f and render.f.
- Verify per docs/forth.md Signature-token-changes-need-direct-smoke-probes: test ATOM-TOK?, TOK-TYPE, renderer output directly before rebuilding; a malformed atom rejects.
- Dep: none (foundational checker-track start).
