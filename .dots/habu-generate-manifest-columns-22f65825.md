---
title: Generate manifest columns from scanner
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T10:33:57.118380+02:00"
---

lib/std.manifest duplicates two machine-derivable columns (word, effect) by hand, so every future public word in lib/ reintroduces stdlib-manifest-test drift at a later, more expensive moment - proven 2026-07-27 when four VEC: rows went missing for days and were found by a worker on an unrelated dot. Owned result: a checked Habu refresh tool (bin/hb --load, package-owned, in tools/) that runs the public-signatures scanner over the manifest's own kind=module rows and rewrites ONLY the word and effect columns from that emission, preserving the human-owned columns (notes, status, owner, test, doc, gate) byte-for-byte, plus a --check mode that exits nonzero on any derivable-column drift so it can join a gate list. It must assert row anchors and refuse unknown or ambiguous rows rather than guessing. This removes the failure mode instead of repairing instances; stdlib-manifest-test remains the independent cross-check. Dependencies: public-signatures scanner (packaged as PUBLIC-SIGNATURES in the vecmem lane, 97642e1c). Acceptance: on a tree with a deliberately drifted effect column, --check exits nonzero naming the row, refresh restores it byte-identically to the scanner emission while leaving human columns untouched (proven by diff), stdlib-manifest-test green before and after, both diff lints.
