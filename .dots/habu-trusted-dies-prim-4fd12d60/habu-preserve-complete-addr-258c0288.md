---
title: Preserve complete address rows through AOT artifact IO and merge
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T14:51:13.059918+03:00"
---

Cedar review of 5226a994, 2026-09-13: aot-decl.f XTOFF-ROW is 8 and aot-capture.f writes location plus metadata, but aot-file.f SEC-LEN/SEC-ROW/SEC-CAP/RESTORE-COUNTS/BASES-AFTER-HOST/MERGE-ROWS still use 4. On engine 28e11361, the new artifact round-trip fixture writes two rows as an eight-byte section; clearing the live row buffer before READ loses the second row while roundtrip=ok still prints. Fix all serialization widths and merge rebasing by cell-location tag and target kind; preserve null metadata and fixed engine offsets. Test full rows from cleared buffers and a real merge with code, DATA and null targets. Reproducer: test/aot-artifact-roundtrip.f with XTOFF-BUF zeroed before FORGET-COUNTS/READ. Unassigned.
