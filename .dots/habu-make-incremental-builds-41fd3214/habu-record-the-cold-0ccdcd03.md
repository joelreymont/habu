---
title: "Record the cold load's requests for the tool role"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T02:30:10.843468+02:00"
---

Problem (tool-role design, 2026-11-01): in the engine support program, a file the cold load reads after t0.f closes (the tail of native-runtime.f) or a cold-load driver file, when the tool requests it without reading it, feeds only its own bytes to TOOL-ROLE, not the files it required: the cold load records no queries, so their closure is unknown. Today the set is src/lib/executable-build.f (requires nothing), tools/build-producer.f and tools/build-target.f (require only pre-t0 files), so nothing is missed, by inspection rather than by construction. Fix at the owner: the cold load records queries and computes FILE-HASH-style hashes (a CANON hook in COLD-SOURCES:LOAD, sharing NDEF-CACHE's HASH-OPEN/HASH-ROW/HASH-CLOSE), so a cold row's hash covers its requires. Depends: lane 'Record the build tool's role' (carl-objk). Claim: unassigned.
