---
title: Measure whether the graph wire form carries every effect
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T17:59:34.394441+02:00"
closed-at: "2026-11-06T18:12:47.436144+02:00"
close-reason: "done: counts in the finding; the facts-format design takes them up (dot habu-design-the-facts-bd8296b9)"
---

Problem: the facts-format design (dot habu-design-the-facts-bd8296b9, /tmp/jerry-big/design-facts-r1.md) saves each word's certified graph in the wire form ASIG-GRAPH-COPY-RECORD writes (src/core/checker.f), which copies each node's bytes as the checker holds them and dies on exception rows in quotation nodes and recovery rows; how many of the 24,497 effects it refuses, whether a copy imports back the same, and the bytes are not known. Acceptance: counts per class (exception-rows, recovery, bad-tag, copyable) and verdict (same, different) on /tmp/jerry-gate/gen2, byte sums (all, distinct), counts of masks, storage facts, created effects and parse counts. Files: probe only (/tmp/jerry-gfid; never lands). Verify: the counts. Depends: none. Ownership: workspace jerry-gfid. Claim: agent=worker workspace=.jj-ws.noindex/jerry-gfid (brief /tmp/jerry-big/brief-gfid.md).

Finding 2026-11-06 (probe /tmp/jerry-gfid/gfid.f on /tmp/jerry-gate/gen2, output /tmp/jerry-gfid/rows.tsv, 7.48 s wall): all 24,497 effect records copy through the wire form and import back structurally the same (GQ-REC?, node by node; E-CONTENT-SAME? cannot judge it because the import makes fresh nodes; negative controls tell effects apart); 0 exception-row, recovery, bad-tag, cycle or bad-offset refusals. The first control died only because `dup` has no checker record (USIG-NEWEST 0). Bytes: 10,511,235 for all blobs (average 429, largest 6,824, make/unmake), 2,185,324 for the 2,920 distinct ones. Not carried by the wire today: 857 nonzero drop masks (the importer passes zero masks) and 15 created effects; this engine has 0 return masks, storage kinds and parse-immediate counts.
