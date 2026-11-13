---
title: "Lay out a stripped program's live data"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:28:05.964458+01:00"
---

Problem: a stripped program's data layout is the build space's, dead bytes zeroed in place (src/habu/link-copy.f FILL-COPY, CUT-DEAD), so a byte any library adds moves later addresses. Acceptance: the image's data is the header, then the live non-body extents in build order under a monotone map (DATA-OFFSET), then the live string bodies sorted by length and bytes, then the kept address-cell list; LAY gives every body one byte past its text; Wasm's DP cell is the laid-out end; two CAST: words added to src/lib/task.f leave the reproducer byte-equal apart from its signature. Depends: habu-keep-a-stripped-d3015a90. Claim: unassigned.
