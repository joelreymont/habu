---
title: Report a ( in a declaration body from check.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T00:30:22.317627+02:00"
---

Problem: 'ENUM kind VARIANT first ;VARIANT ( c ) ;ENUM' and the same in a STRUCTURE body die rc 70 at load ('unexpected token in enum declaration at ('), and tools/check.f prints only that load message, with no packet, while a \ in a compact ENUM gets an E-BAD-DECLARATION packet (probes on the stack's hb, 2026-11-06). tools/check-core.f CHK-DECL-RAW-BODY (:1197) and src/habu/verify-source.f DECL-TOKEN (:2573) exist so the checker's replay never accepts a body the engine refuses. CHK-DECL-RAW-BODY starts its slice at the lexer's first body token, so a comment between the family name and the first body token is never replayed, and placement may decide what is reported (a \ in a STRUCTURE body is unmeasured). Acceptance: each comment form and placement the load refuses in a STRUCTURE or ENUM body gets an E-BAD-DECLARATION packet from tools/check.f, with a refused-program test per form; moot if habu-accept-comments-inside-145f82eb makes bodies accept comments. Files: tools/check-core.f, src/habu/verify-source.f, the declaration diagnostics tests. Verify: those tests. Depends: incremental builds (Joel 2026-11-04: the check tool's work waits) and habu-accept-comments-inside-145f82eb. Ownership: the check tool's declaration replay. Claim: unassigned.
