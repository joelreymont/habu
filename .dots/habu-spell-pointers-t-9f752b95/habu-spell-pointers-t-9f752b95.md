---
title: Give strings a checked role
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T12:52:54.599921+02:00"
---

Problem: a string (`^u8 n`, docs/forth-card.md:114) is two stack items with no role, so the checker accepts a rev where a path goes and a reader cannot tell a string from a buffer. Goal: a string is one item, `str`; a role is a NEWTYPE or DEFTYPE over it, which wrap a value of any width (`NEWTYPE path str`, `DEFTYPE REV str`, approved by the user 2026-11-15); one spelling across the tree. Design (approved 2026-11-15): every file is on the str spelling and carries the `split-str` marker; what remains lands on master as a stack, each commit built by the one before (the user's choice): the moves, any file in any order, each dropping its marker; then the marker's deletion and docs/forth-card.md:114 rewritten. While a file is marked, its diagnostics print a str as ^u8 n, and only a file loaded by name carries the marker: one read from stdin cannot. Acceptance: no file carries the marker, the marker is deleted, docs/forth-card.md:114 spells a string str, and the full suite passes. Depends: none. Claim: unassigned.
