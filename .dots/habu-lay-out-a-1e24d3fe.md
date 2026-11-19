---
title: Lay out a deeply nested sum in linear time
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:04:25.862253+01:00"
---

Problem: TYPED-VARIABLE or 1 TYPED-BUFFER of a NEWTYPE over option nested k deep takes 0.25 s at k=512, 0.92 s at 1023, 2.02 s at 1536 and 3.53 s at 2047, while the NEWTYPE and two words calling it take 0.08 s at 2047. Measured causes: CAST-SEEN? compares each seen cast row in turn (src/core/checker.f, workspace carl-castseen); HAS-ADDRESS? and HOLDS-SCRATCH? (src/core/type-family.f) work out a width per level through TFAM-STORAGE-FIELD and TFAM-STORAGE-PAY and drop it (carl-deepcut2). Master's engine also stops from k=2046 with E-STACK-BOUNDS in SUMV-SLICE-ROW?, where the deep-cut engine runs 2047. Acceptance: k=2047 runs, and the times per doubling of k stay near 2x from 512 to 2047. Depends: none. Claim: agent=carl workspaces=.jj-ws.noindex/carl-castseen .jj-ws.noindex/carl-deepcut2.
