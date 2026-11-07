---
title: Unmap the address-cell index a build maps
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T01:59:09.828882+02:00"
---

Problem: EMIT-MARK's index is mapped by INDEX-BUILD (src/habu/habu2.f, after :7779) and recorded at INDEX-CELL in the build space's header; nothing in linker.f, link-band.f or link-data.f unmaps it, and LINKER's UNMAP frees only the space, so each LINKER:WITH build in one process leaks one mapping, compiled or linked files alike (found by reading in lane D's review, 2026-11-07; not run). Verify first: count the process's mappings (vmmap) across repeated builds in one process, e.g. test/fixtures/file-objects/build.f stop mode's twelve builds. Acceptance: the close unmaps the index the open mapped; a process running N builds holds no more mappings than one running one. Ownership: lead decides after the measurement. Claim: unassigned.
