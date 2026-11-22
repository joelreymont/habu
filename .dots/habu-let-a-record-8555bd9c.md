---
title: Let a byte view take a closed record
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T15:04:35.442732+02:00"
---

Problem: src/lib/arena.f keeps 'self ^u8' and casts chunk links back (>CHUNK) to free mapped bytes, though BYTE-VIEW takes a pointer to a closed record. Acceptance: arena.f's self field and chunk casts go; the arena tests pass. Depends: none. Claim: unassigned.
