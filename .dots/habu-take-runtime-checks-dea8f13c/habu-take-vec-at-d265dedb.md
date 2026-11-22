---
title: "Take VEC:AT's bounds check out of default code"
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:54:11.178664+01:00"
---

Problem: src/lib/vector.f:85-88 VEC:AT throws E-VEC-BOUNDS on an index outside 0..COUNT-1 on every call, a check against a program error that default code must not run (AGENTS.md Changes; the parent's acceptance). Callers whose index is in range by construction pay it on every access (xml/source.f SRC-MARKS and SRC-MARK-AT, byte-edit.f:69, every table row); callers that pass an index from outside (src/lib/word-docs.f NAME$, word-docs-test.f:102 and :122; source-lex.f:87-89) rely on it as their contract. Acceptance: VEC:AT does no bounds test in default code; each caller whose index comes from input states and tests its own range (an input check, not a program-error check); vector-test.f:46-54 and :98 move under the --checks build (habu-add-the-checks-a49d054d) or are disabled with that dot's id. Depends: none. Claim: unassigned.
