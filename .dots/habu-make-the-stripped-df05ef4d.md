---
title: Make the stripped link linear in the closure size
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T09:02:57.342889+03:00"
---

Problem (measured by the reach study, recorded beside src/habu/aot-lib.f TEXT-ADR,): OLD>NEW and MEMBER-AT in src/habu/aot-lib.f scan every closure member per relocated instruction, so the stripped link is quadratic in NCLO. A build past 1 MiB of code costs minutes at test scale, which is why no fixture pins the far-ADR case and why Tender's standalone link time grows with its closure (Tender standalone: 1.36 MB of code, built by aspen). Acceptance: measure the link time of the Tender standalone build and of tools/hb-build-test.f's largest stripped fixture before; replace the per-instruction member scans with a lookup filled once per link (a member table sorted by entry with binary search, or a per-offset map over the closure); every image byte-identical before and after (cmp on the hb-build-test fixtures and the Tender image); link times reported after; then state in TEXT-ADR,'s comment whether a >1 MiB fixture is affordable and, if it is, add it (a stripped image past the ADR window pinned by tools/hb-build-test.f). The link is a compiler pass, so the before/after measurement is required, not optional. Files: src/habu/aot-lib.f, tools/hb-build-test.f. Verify: tools/hb-build-test.f; test/run.f; Tender's build with aspen. Depends: none. Ownership: hazel. Claim: unassigned.
