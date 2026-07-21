---
title: Cross-check remapped coordinates
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:06.090947+02:00"
closed-at: "2026-07-21T22:08:46.748665+02:00"
close-reason: Superseded by direct authenticated source-frame provenance. Diagnostics no longer remap coordinates from flat composed source.
---

Full context: DIAG-REMAP silently trusts byte_start over supplied line/column, ignores a text byte suffix, accepts malformed include_chain types, and cannot remap zero-width EOF through JSON or text line-column paths. Cross-check every supplied coordinate representation against the authenticated map; require include_chain to be an array of strings before replacement; route byte and line-column EOF through one span-aware lookup. Acceptance: JSON/text mismatches, malformed chain, negative/out-of-range and noncanonical numeric text reject; byte EOF, line-column EOF, text EOF and hb-build EOF diagnostics remap exactly. Files: tools/source-map.f/test and tools/diag-remap.f/test.
