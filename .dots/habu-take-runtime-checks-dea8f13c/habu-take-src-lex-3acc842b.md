---
title: "Take source-lex TABLE@'s null check out of default code"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T13:56:50.207467+01:00"
closed-at: "2026-11-22T14:15:08.726635+01:00"
---

Problem: src/lib/source-lex.f:87-89 TABLE@ throws E-VEC-BOUNDS when a token table is not made, on every token access (LINT-LEX's public token words at :190-216 and the lexer's own pushes); a token word called before SOURCE or after RELEASE is a program error, which default code must not check (AGENTS.md Changes). Acceptance: TABLE@ no longer tests for NULL-PTR in default code (the word goes if it is then a bare @); a test of the null case moves under the --checks build (habu-add-the-checks-a49d054d) or is disabled with that dot's id. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-slx.
