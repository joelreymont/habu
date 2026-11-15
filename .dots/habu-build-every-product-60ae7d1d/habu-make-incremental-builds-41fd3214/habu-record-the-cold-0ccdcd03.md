---
title: "Record the cold load's requests for the tool role"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-01T02:30:10.843468+02:00"
closed-at: "2026-11-04T11:48:49.049763+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (docs/design.md \"Machinery being replaced\"). cold load (tools/cold-load.f) and saved support-image tool role; both replaced by the cross-build and file linker"
---

Problem (tool-role design, 2026-11-01): in the engine support program, a file the cold load reads after t0.f closes (the tail of native-runtime.f) or a cold-load driver file, when the tool requests it without reading it, feeds only its own bytes to TOOL-ROLE, not the files it required: the cold load records no queries, so their closure is unknown. Today the set is src/lib/executable-build.f (requires nothing), tools/build-producer.f and tools/build-target.f (require only pre-t0 files), so nothing is missed, by inspection rather than by construction. Fix at the owner: the cold load records queries and computes FILE-HASH-style hashes (a CANON hook in COLD-SOURCES:LOAD, sharing NDEF-CACHE's HASH-OPEN/HASH-ROW/HASH-CLOSE), so a cold row's hash covers its requires. Depends: lane 'Record the build tool's role' (carl-objk). Claim: agent=jerry, not started.

Second gap of the same kind (lane A review, 2026-11-01): the support child's program arrives on stdin, and its `require tools/cold-load.f`, `require tools/tool-role.f` and `require tools/native-build-support.f` search the working directory before the engine root with no recorder installed (the old NB-START recorded the last two; lane A dropped it). For a child started outside a tree the first candidate `<cwd>/tools/tool-role.f` is missing and unrecorded, so a file created there later would not mark the saved support image stale (probe /tmp/carl-role-rev/probe/stdin1.txt). TOOL-ROLE:ENTRY cannot open a window at stdin top level (ACTIVE-SOURCE dies with "include: depth underflow"). The fix records these lookups at the owner along with the cold load's requests.
