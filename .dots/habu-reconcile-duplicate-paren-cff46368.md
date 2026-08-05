---
title: Reconcile duplicate paren-lexer fixes
status: active
priority: 1
issue-type: task
created-at: "2026-07-29T18:20:48.532645+02:00"
---

Full context: master and this branch independently implemented the SAME fix to the shared lint lexer — the standalone-paren rule, where an open paren opens a comment only when followed by a delimiter or end of input, so a word legitimately named with a leading paren lexes as a WORD. Master spells it ENGINE-DELIM? plus PAREN-STANDALONE? and retired RAW-WS?; this branch spelled it PAREN-OPEN? plus ROW-PAREN? over RAW-WS?. The merge took MASTER's version wholesale as canonical, which is correct for the shared rule but DROPPED two things this branch had: (1) the print-paren support, where .( ... ) lexes as one inert COMMENT token — master's lexer has none, and tools/package-diff-lint-core.f OPAQUE? throws on any token kind it was not taught, so a file containing .( is a live hazard for that consumer; (2) the branch's own fixtures TEST-LEXER-PAREN-WORD and TEST-ROW-PAREN-FIELD, which were replaced by master's test file. Re-add print-paren support on top of master's ENGINE-DELIM? shape, restore equivalent fixtures, and check whether master's fix covers the row-body path (SKIP-INERT) as well as the main loop. Acceptance: a word named with a leading paren lexes as WORD in both the top-level and row-body paths; .( is one COMMENT token; the reinstated fixtures red if either rule is reverted.

ESCALATED 2026-07-29 — this is a LIVE RED, not a latent hazard. Taking master's
lexer dropped the print-paren support that tools/error-code-lint-test.f depends
on, and that suite now fails in the gate: phase lint-tools/error-code,
assertion 19, expected 0 got 1, with the diagnostic "unterminated string
literal". The fixture is the print-paren case `.( -9001 constant E-XA )
-9001 constant E-XB`, which must report 0 findings because a print-paren body is
not code. Master's ENGINE-DELIM? shape has no notion of `.(`, so the body is
lexed as ordinary words and the trailing quote handling breaks. Re-adding
print-paren support on top of master's shape is therefore required to get the
suite green again, not merely desirable.

Claim: agent=parenrec workspace=.jj-ws/habu-reconcile-duplicate-paren-cff46368
