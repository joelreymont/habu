---
title: Update checker capture fixtures
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T19:50:26.220839+03:00"
---

The checker-surface and pointer-storage fixtures expect HIDX-BUILD, CORE-FOLD-C and NULL-PTR-CELL records to be absent, but the matching product names them while still refusing checked calls. The census expects six retired symbols to lose their contracts, although live occurrences retain exact contracts by ordinal. Updated fixtures pass on matching product/whitebox engines, including reclamation of 288 bytes after actual occurrence trim; independent Astra review found no issues. Landing waits for the ordinal-contract implementation: master at 796d3f6b lacks the occurrence roots in CHECKER-SWEEP:MARK-ROOTS. Rebase this fixture change after that implementation lands, rerun the three rows, and close with the landing. Owner and primitive top-row boundary failures remain separately owned.

Reviewed fixture1bffc97e is integrated at8910d29b with its ordinal-contract prerequisite. Matching release-source35e00c85 product4653470c passes checker-surface and pointer-storage; its same-source whitebox product passes effect-store-census, including actual288-byte reclamation. The census is a WHITEBOX-SUITE and cannot run on the sealed product. The completed fixture workspace and products are retired. Combined fixed-point native registry and landing remain open.
