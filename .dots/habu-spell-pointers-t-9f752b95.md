---
title: Give strings a checked role
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T12:52:54.599921+02:00"
---

Problem: a string (`^u8 n`, docs/forth-card.md:114) is two stack items with no role, so the checker accepts a rev where a path goes and a reader cannot tell a string from a buffer. Goal: a string is one item, `str`; a role is a NEWTYPE or DEFTYPE over it, which wrap a value of any width (`NEWTYPE path str`, `DEFTYPE REV str`, approved by the user 2026-11-15); one spelling across the tree. Acceptance: the approved design implemented, every .f in the tree on the new spelling, the full suite passing. Depends: none. Claim: agent=lead workspace=.jj-ws.noindex/habu-spell-str.
