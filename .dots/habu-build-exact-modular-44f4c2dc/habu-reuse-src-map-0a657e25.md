---
title: Reuse source map storage
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:38.859787+02:00"
closed-at: "2026-07-21T22:08:46.759518+02:00"
close-reason: Superseded by owned immutable authenticated source frames; flat source-map storage will be deleted.
---

Full context: SOURCE-MAP allocates decoded paths on every OPEN, caps source/map files at SOURCE-ARENA-CAP, and only clears pointer vectors. After the growable buffer and habu-validate-canonical-src-3fbbcf67 land, own source bytes, map bytes, decoded-path arena and row/chain vectors; clear for reuse, release replaced mappings, and preserve authenticated parse atomicity. Acceptance: maps and sources above 1 MiB open; repeated OPEN for stdout/stderr and failures has stable mapping count; a failed OPEN leaves no partially authoritative state; canonical roundtrip remains green. Files: tools/source-map.f/test. Full fan-in prerequisite: habu-validate-canonical-src-3fbbcf67.
