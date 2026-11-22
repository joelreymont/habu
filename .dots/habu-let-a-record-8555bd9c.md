---
title: Let a byte view take a closed record
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T15:04:35.442732+02:00"
---

Problem: BYTE-VIEW's primitive row (src/habu/prims.f:385) gives its pointee the default kind, which refuses every declared record (src/core/checker.f:4529-4538, :4548-4566), so a C struct held as a record reaches its foreign call only through its first field's address, and src/lib/arena.f keeps 'self ^u8' and casts chunk links back (>CHUNK) to free mapped bytes. Acceptance: BYTE-VIEW takes a pointer to a closed record or family, an OPAQUE one only inside its declaring package (rejected outside with the package named); the row change lands staged (a new primitive-spec atom, docs/bootstrap.md "Staged hosts"); arena.f's self field and chunk casts go; base-pointer-arith-refusals passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-bview.
