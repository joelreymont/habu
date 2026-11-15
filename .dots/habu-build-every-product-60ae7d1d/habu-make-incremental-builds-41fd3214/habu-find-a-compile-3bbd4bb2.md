---
title: "Find a compile's stores without comparing all earlier data"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:39:52.114041+02:00"
---

Problem: as a file's compile or a new part's starts, src/habu/linker.f COPY-BELOW (:2220, called at :4989 and :5089) copies every cell from XTCELL-END to FILE-DP (BELOW, :2132), mostly earlier files' data, and every earlier record; after the compile FIND-STORES (:2376) and CHECK-EARLIER (:2266) compare them, and SEALED-N (:2478) and SEAL+ (:2481) scan every earlier record. The cost grows with pieces times data: the copy and compare of earlier files' data and records measured 12.5 percent of a cold engine build (2026-11-10), over the 5 percent bar. Most of the copy's cost is its one guarded store per cell (src/core/bytes.f:20 BYTE-COPY-LEN). The cut is a copy that checks its destination once: BYTE-COPY-LEN as an engine word on arm64, as it already is on Wasm (src/arch/wasm/kernel-map.f:35 maps it to WKWORDS:COPY-LEN), or T0 leaving the guard off a store (dot habu-t0-skip-the-4da2e9e2, which does not cover a computed destination). Either is an engine change, so it is a design for Joel before a worker. Records change between compiles (the build driver moves earlier records to another wordlist before lower-cert-base.f and declaration-transaction.f), so a snapshot kept across pieces is not safe for records.

Acceptance: COPY-BELOW and FIND-STORES together under 5 percent of a cold engine build by the build profile (`bin/hb --load tools/build-profile.f`); warm equal to cold; file-objects passing, STOP-GAP's plain store into a gap still refused.

Claim: unassigned.
