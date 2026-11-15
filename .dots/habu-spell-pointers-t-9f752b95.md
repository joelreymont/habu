---
title: Spell pointers ^T and give strings a checked role
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T12:52:54.599921+02:00"
---

Problem: a pointer type reads as two stack items (ptr client), and a string (ptr u8 n, docs/forth-card.md:114) carries no role, so the checker accepts a rev where a path goes and a reader cannot tell a string from a buffer. Goal: pointers spelled ^T, one token per stack item; a string is one item, `str`; a role is a NEWTYPE or DEFTYPE over it, which wrap a value of any width (`NEWTYPE path str`, `DEFTYPE REV str`, approved by the user 2026-11-15); one spelling across the tree. Acceptance: the approved design implemented, every .f in the tree on the new spelling, the full suite passing. Depends: the design (Fable), the JSON header mode and CURL:RESET lanes landing first. Claim: agent=lead workspace=.jj-ws.noindex/habu-spell-pointers-t-9f752b95 (integration).
