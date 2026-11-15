---
title: Give strings a checked role
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T12:52:54.599921+02:00"
---

Problem: a string (`^u8 n`, docs/forth-card.md:114) is two stack items with no role, so the checker accepts a rev where a path goes and a reader cannot tell a string from a buffer. Goal: a string is one item, `str`; a role is a NEWTYPE or DEFTYPE over it, which wrap a value of any width (`NEWTYPE path str`, `DEFTYPE REV str`, approved by the user 2026-11-15); one spelling across the tree. Design (approved 2026-11-15): the str type with >str and str> and wide roles land first (NEWTYPE name base and DEFTYPE NAME base with the base on the definer's line, DEFTYPE making >NAME and NAME>BASE; CAST: of any width with equal cell counts). The rest lands on master as a stack, each commit built by the one before (the user's choice): an engine that reads a per-file marker, split-str, under which a file reads str as ^u8 n; one mechanical commit turning every row, literal and string keyword to str and marking every file (pairs that are a buffer and its capacity, FUNCTION:, CAST: and LINEAR: lines stay pairs); the moves, any file in any order, each dropping its marker; then the marker's deletion and docs/forth-card.md:114 rewritten. Acceptance: the approved design implemented, every .f in the tree on the new spelling, the full suite passing. Depends: none. Claim: agent=lead workspace=.jj-ws.noindex/habu-spell-str.
