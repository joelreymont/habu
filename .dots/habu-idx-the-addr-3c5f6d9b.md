---
title: "Index the address-cell registrar's row scan"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T11:09:21.785913+03:00"
---

Problem: src/habu/habu2.f EMIT-MARK scans the live SNAP-RELOC:XTCELL rows on every declared address cell, so registration is O(rows) per declaration and O(rows^2) over a build; the engine's own closure is 25454 rows on 2026-09-13 (was 19088 when the cap was set) and the cap is now 65536 (habu-size-the-snapshot-1ca5db10), so both the scan and the table's headroom are worth a structure: an index over the rows (a hash or a sorted view kept by the registrar) so a declaration costs O(1) or O(log rows), and a measured build-time number before and after. Acceptance: the registrar's per-declaration cost no longer scales with the row count; the two-generation chain and byte identity unchanged; the 40000-cell regression and the snapshot suites green; tools/compile-floor.f or a build-time stopwatch quotes the difference on a quiet box. Files: src/habu/habu2.f, src/habu/layout.f. Verify: the suites, the chain, the measurement. Depends: habu-size-the-snapshot-1ca5db10. Ownership: hazel. Claim: unassigned.
