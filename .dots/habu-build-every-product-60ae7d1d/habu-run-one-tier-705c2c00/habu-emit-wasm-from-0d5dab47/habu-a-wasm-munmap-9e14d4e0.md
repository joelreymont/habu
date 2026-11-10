---
title: "A Wasm munmap refuses address 0; test map-anon's other refusals"
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T19:22:00.055453+02:00"
---

Problem (feature review A, F4 and F5): src/arch/wasm/link-routines.f UNMAP-BODY :520-523 accepts address 0 (page-aligned, below 2^32); INSERT-RUN, writes the node at 0 and ctx's free head becomes 0, which reads as none, so every free run is lost. Tier 1's UNMAP-ROW does the same. NULL-PTR is the one such address checked code makes without a cast; any other address inside the module's own pages stays the caller's trust, as natively. map-anon's refusals are tested for a count of 0 only. Acceptance: munmap of address 0 answers -1 and changes nothing; test/wasm/hb-build.f m.f checks it, a negative count, a count above 2^32 and a refused growth (1 32 lshift map-anon answers 0 -1), and munmap of a count of 0. Files: src/arch/wasm/link-routines.f, test/wasm/hb-build.f. Verify: engines converge; the test ends test: ok. Depends: none. Ownership: those files. Claim: unassigned.
