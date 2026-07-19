---
title: Structure trust inventory
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:31:49.523543+02:00"
---

tools/trusted-inventory.f:39-43,186-190,269-340,1399-1404 stores each trust site in nine raw columns, each classification in eleven, and each owner in four arrays. Every offset, length, line, class, match count, and index is n, so column/call-site swaps can associate a site with the wrong effect, class, path, or owner while the security inventory still emits a plausible row. CLASS$ recognizes four values then maps every unknown/corrupt code to TRUST-BARE, masking table corruption as a legitimate class. Define trust-site-kind ENUM and STRUCTURE trust-site, classification, and owner-reference rows with typed arena spans/ids/counts in LAYOUT-BUFFER. Append each row transactionally after capacity/arena preflight; dispatch every class through exhaustive MATCH and hard-fail invalid internal state. Preserve source tokenization, site identity/order, baseline/strict/ratchet counts, owner resolution, output bytes, and all policy classifications. Add checker negatives for every semantic field/id swap, invalid-kind failure, canary/full-capacity/arena rollback, duplicate/dead/unmatched owner cases, sort stability, and exact inventory snapshots/ratchets. Measure source helpers removed, JIT/DATA/CODELEN, table bytes, and scan/sort/report throughput before/after. Files: tools/trusted-inventory.f and focused tests. Verify trusted-inventory strict/baseline, trust lint, package/host/filemap/dot/fixpoint/full native gates. Coordinate classification-content dots; ownership here is typed rows and invalid-state handling.
