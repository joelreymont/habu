---
title: "Add ARENA:WITH, a scope whose chunks are released together"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:47:43.194606+02:00"
---

Problem: Maki's operations runner (maki src/ops.f, relayed by tim 2026-11-06) needs per-run storage released when the run returns or throws. tim relays Joel's 2026-10-26 ruling that Habu has one arena: chunks that never move, released together; a later relay to tim (transcript c650c2e8) gives Joel's names: package ARENA with ARENA:arena, ARENA:WITH and ARENA:ALLOC ( ptr ARENA:arena n -- ptr u8 n ), and ARENA:OPEN, RESET and CLOSE with PG. Master has only ARENA-DECL's `ARENA NAME t` definer (src/core/arena.f) over DYNAMIC-STORAGE's chunks (src/core/dynamic-storage.f); the prototype in tim-lex's src/lib/memory.f:290-401 (ARENA:WITH ( R [ R ptr arena -- S ] -- S ), ARENA:ALLOC zeroed, cell-aligned, never moving, live until the scope closes) never landed, and its design file (~/.cache/tmp/tim/chat-tools/design-one-arena-merged.md) is gone. Until this lands Maki's runner uses DYNAMIC-BUFFERs released in RUN's finally, as src/lib/fsm.f does, and moves to ARENA:WITH after. Acceptance: package ARENA with ARENA:WITH and ARENA:ALLOC as named: a scope's allocations are zeroed, never move, and are all released when the scope's quotation returns or throws; a nested scope releases only its own. Check the names and contracts against Joel's recorded words before design; the design and its public list go to Joel. Caller: Maki ops (tim). Files: src/lib/memory.f or a new src/lib/arena.f, src/core/dynamic-storage.f, a test beside it. Verify: the new test; Maki's ops runner on it. Depends: none (not on the incremental-builds path). Ownership: unassigned. Claim: unassigned.

Consumers: Maki's operations runner keeps 69 dynamic buffers and a hand-kept release list in its place (src/ops.f:6048-6067).
