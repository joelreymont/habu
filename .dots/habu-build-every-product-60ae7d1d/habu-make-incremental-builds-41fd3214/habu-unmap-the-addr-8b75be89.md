---
title: Unmap the address-cell index a build maps
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T01:59:09.828882+02:00"
closed-at: "2026-11-07T03:55:56.985646+02:00"
close-reason: "landed as 409a3b44 'Release a build's address index at its close', reviewed ACCEPT; full suite with candidate rc 0, 293 of 293 suites"
---

Problem: EMIT-MARK's index is mapped by INDEX-BUILD (src/habu/habu2.f, after :7779) and recorded at INDEX-CELL in the build space's header; nothing in linker.f, link-band.f or link-data.f unmaps it, and LINKER's UNMAP frees only the space, so each LINKER:WITH build in one process leaks one mapping, compiled or linked files alike (found by reading in lane D's review, 2026-11-07; not run). Verify first: count the process's mappings (vmmap) across repeated builds in one process, e.g. test/fixtures/file-objects/build.f stop mode's twelve builds. Acceptance: the close unmaps the index the open mapped; a process running N builds holds no more mappings than one running one.

Measured 2026-11-07 (/tmp/carl-unmap/probe.f and empty.f, candidate /tmp/l4i.1jt8/hb on carl-l4lw, vmmap after N builds in one process): N builds that load test/fixtures/file-objects/count.f and mark.f hold 176 KB more untagged memory at N=12 than at N=1, 16 KB per build; N empty builds hold the same (286,144 KB at 1 and 12). The close calls INTERN:CLEANUP (linker.f:1964), so the per-build page is inferred, not traced, to be the index the marks map. Fix in LINKER's close, after L4 lands (linker.f is L4's): release the mapping the space header's INDEX-CELL holds before the space is unmapped, then the probe shows no growth. Ownership: src/habu/linker.f. Claim: agent=worker workspace=.jj-ws.noindex/carl-unmap (brief .out/brief.md there).
