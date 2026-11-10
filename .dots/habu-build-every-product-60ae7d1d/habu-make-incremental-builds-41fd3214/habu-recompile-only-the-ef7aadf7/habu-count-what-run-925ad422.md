---
title: Count what run words reach, word by word
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T11:29:37.962711+02:00"
closed-at: "2026-11-10T11:36:35.178490+02:00"
close-reason: "measured: per run word median 2, p90 9, max 7529; per file union median 15, p90 536, max 7580; 0 later-record references; 496 of 496 piece data starts 8-aligned; log carl-recomp/.out/r/z.log; S3 saves the list of reached words, no newest time or clock guard (design-word-level.md)"
---

Problem: the design's run rule per word (.jj-ws.noindex/carl-layout/.out/design-word-level.md 3.2 rule 2) chooses between one newest time per run word with a clock guard and a list of every reached word; the closure sizes were not determined, nor whether a word refers to a later record of its piece, nor whether a piece's data start is 8-aligned. Acceptance: a probe engine build of the engine reports, per run word and per file, how many words its code reaches transitively (median, 90th percentile, maximum), the count of references to a later record of the same piece, and the count of pieces whose data start is not a multiple of 8. Files: probe prints in .jj-ws.noindex/carl-recomp src/habu/linker.f, never committed. Verify: the numbers with the log they came from. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-recomp
