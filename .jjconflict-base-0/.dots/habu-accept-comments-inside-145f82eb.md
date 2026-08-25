---
title: Accept comments inside type declaration bodies
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:40:47.454460+02:00"
---

Capability gap measured by the wave B4 lane, uniform and pre-existing across all three definers: a backslash comment or paren comment on a FIELD or VARIANT line inside a declaration body rejects - legacy SUMTYPE throws 7109, unified ENUM and STRUCTURE throw 7107 unexpected token - so payload fields cannot be documented at their declaration site and every documented-payload migration lane moves its notes above the declaration block. Behavior: the shared type-DSL token reader skips comment tokens inside declaration bodies exactly as the outer compiler does, for the unified ENUM and STRUCTURE front ends (the legacy definers retire with the FINAL migration lane and do not need the capability). Hostile fixtures: a comment token must not become a field name, a variant name, or an arity; a comment containing the exact text FIELD x n must not declare anything; an unterminated paren comment inside a body must reject with a diagnostic naming the comment, not a bare 7107. Acceptance: positive fixtures with commented FIELD and VARIANT lines through both unified front ends; the hostile fixtures red; enum-decl and structure suites green. Owner: the shared declaration token reader in src/core (locate via enum-decl.f/structure-decl.f common path). Dependencies: none.
