---
title: Exclude compiler literal rows from stripped DATA
status: open
priority: 2
issue-type: task
created-at: "2026-09-23T16:34:20.281941+03:00"
---

Problem: the stripped window opens before NSTR:WINDOW-OPEN, so it includes the compiler owner and two 8192-cell literal lookup tables. The current Tender 806f0654/feb55f7d probe attributes 12571 encoded bytes and 131112 bytes of restored DATA extent to that metadata. Hazel confirms SOURCE-ROWS is a build-driver interface, not stripped runtime state. Fix: open the pool first and start the stripped window at NSTR:SOURCE-SPAN. Preserve literal bodies and application DATA; a persisted compiler-metadata pointer must refuse by its owning cell, not silently read zeros. Acceptance: stripped-literal red-before metadata refusal, all existing positive literal/quotation/alias/stripped fixtures green, same-pair Tender file/DATA measurement before and after. General unused application DATA reachability remains 5b7d02bb, including its conservative-alias requirement. Ownership: alder. Claim: agent=alder workspace=.jj-ws/alder-data-root. No snapshot compression, capacity reduction or engine-format change.

Implemented on 31bcf100. The new stripped-literal fixture fails on the base
(expected rc 70, got 0) and passes with the new boundary; the refusal names
SLT-COMPILER-ROWS and the unrestored engine address. All eleven focused registry
rows pass on a private faa44fcc engine: stripped-literal, stripped-quotation,
stripped-address, stripped-entry, stripped-sparse-data,
stripped-lifecycle-prepare, hb-build-fixtures (all four files in one load),
hb-build-stripped, hb-build-stripped-cells, native-gate-aot-positive and
native-gate-aot-negative. Evidence: ~/.cache/habu/data-root/source-31bcf100/.
This maker file is not baked; full integration gate remains Hazel's chain.

Controlled Tender measurement: same feb55f7d source, 806f0654/274f9bea pair and
paths, with only aot-window-latch.f changed. DATA values 143113 -> 130542,
bitmap 12346 -> 11446, restored window 5821808 -> 5690696 bytes. Code stays
2037592 bytes. File stays 2228416 bytes: alignment absorbs the 13471-byte
payload reduction. No file-size, latency or RSS improvement claimed.
The rebuilt binary passes its DB/HTTP suite and migrate/serve under
MemoryDenyWriteExecute, each using a throwaway database. Evidence and binary:
~/.cache/habu/data-root/source-806f0654/no-literal-rows-* and
tenderd-no-literal-rows (sha256 fef4a5078464df0334b645dfa1d0d8443650bd4bd304e3ee9044d5c1585fa7a5).
