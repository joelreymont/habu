---
title: Put small MEM callers on the heap
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:38:50.520080+02:00"
---

Problem: src/lib/memory.f's header says storage much smaller than STACK-ABI:PAGE-BYTES belongs on HEAP:ALLOCATOR or in an arena, but these callers still map whole pages for small blocks: zip-state.f, spawn.f RECORD-CELLS, process-command.f CTX-CELLS, fs.f WALK-FRAMES0, pg.f, the json scratch, xml/state, secret.f REQUEST-BYTES, jsonl, process-chunks, net/model, mcp, bearer, http-arena, datagram and aio.f G-CHUNK-LEN. Acceptance: none of them takes a MEM mapping below STACK-ABI:PAGE-BYTES. Depends: none. Claim: unassigned.
