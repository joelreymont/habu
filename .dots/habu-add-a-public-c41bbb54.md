---
title: Add a public live-local query on LINT-LEX
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T08:59:18.179357+02:00"
---

Problem: the locals lint copies source-lex.f's private LOCAL$ and redoes its private group tracking (LOCAL-BINDS?) because no public query says whether a name is a live local at a position (cstr landing notes, carl-dots/.out/carl-file/cstr-land-notes.md). Joel approved the public API addition on 2026-10-30 (interview). Acceptance: one public word on LINT-LEX answering whether a name is a live local at a source position, typed (position, name span -- bool), used by the lint; the lint's LOCAL$ copy and LOCAL-BINDS?'s own tracking gone; the lint's existing findings unchanged (691 older findings, 0 disagreements with the lexer's block tracker); a test of the query through the lint on a definition with a reopened block. Files: src/lib/source-lex.f (package LINT-LEX), the lint tool and its test. Verify: the lint's test rows; the lint over src/ unchanged. Depends: none. Ownership: LINT-LEX's locals tracking. Claim: unassigned.
