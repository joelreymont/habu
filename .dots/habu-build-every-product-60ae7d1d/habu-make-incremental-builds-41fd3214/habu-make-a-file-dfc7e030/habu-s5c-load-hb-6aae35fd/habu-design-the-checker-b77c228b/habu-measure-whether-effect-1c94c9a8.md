---
title: "Measure whether effect text round-trips the checker's graphs"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T17:21:18.349531+02:00"
closed-at: "2026-11-06T17:38:15.152829+02:00"
close-reason: "done: the measurement's counts are in the finding; the facts-format design takes them up (dot habu-design-the-facts-bd8296b9)"
---

Problem: design r4 writes each word's facts as declaration text, but src/core/checker.f CHECKER-ASIG-CAPTURE (:10386-10389) says declared text is not replayed because certification can add fixed input requirements; if text loses facts, C2's writer must render the certified graph and the grammar may need to grow. Acceptance: counts per verdict (same/different/unrenderable/parse-refused) over every pool row of /tmp/jerry-gate/gen2, classes of difference with examples, counts per control/defer flag. Files: probe only (/tmp/jerry-fid, workspace jerry-fid; never lands). Verify: the counts. Depends: none. Ownership: workspace jerry-fid. Claim: agent=worker workspace=.jj-ws.noindex/jerry-fid (brief /tmp/jerry-big/brief-fid.md).

Finding 2026-11-06 (probe /tmp/jerry-fid/fid.f on /tmp/jerry-gate/gen2, route A via TRUSTED: bodies; output /tmp/jerry-fid/rows.tsv): gen2 has no signature pool (CK-AOT-SIG-LEN 0); every symbol's record is present, active 1. Of 24,497 effect records, rendered with every family package-qualified and parsed back by USIG-ADD-AS (text without the outer parentheses; `( n -- n )` is refused 7156), 15,324 are the same (E-CONTENT-SAME?; control: one text parsed twice interns to one content offset), 8,814 differ, 359 are refused, 0 unrenderable. Differences: raw-storage type variable TVK-RAW 7,924 (minted by SIG-RAW-MODE for create/variable/constant, checker.f:6163-6180; no source spelling); no-scope storage mark (NSCOPE) 263; pointer provenance kinds TVK-OFF 185, TVK-CELL 8, TVK-NULL 2; base row of callback kind RVK-QUOT 325; callback tail with no-scope mark 68; shared callback rows 30 (`dip`, `bi`, `each`, `map`, `fold`: the renderer prints no row names). Refused: 352 global rows naming families private to another package (comp:vreg 264, comp:label:branch 39, comp:cc 39, mem:chunk 21, mem:frame 12), 7 c2-mem rows. Flags: DEAD 77, THROW 9,278, EXTERNAL 19,326, ZERO-FALSE 1, RENDERS 23, intrinsic bits 8-12 5/6/7/5/2, PARSES 136, CREATES 21, NOMINAL 25, defer 126, NAMES 2, GENERATES 1. Conclusion: today's declaration text cannot carry 37% of the certified effects; the facts format needs a design before C2.
