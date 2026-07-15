---
title: "Add STR:BUF-APPEND-C typed byte append"
status: open
priority: 3
issue-type: task
created-at: "2026-07-15T15:05:05.417897+02:00"
---

Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census. The typed STR: surface wraps BUF-RESET/BUF-LEN@/BUF-APPEND but omits a typed equivalent for the raw single-byte appender BUF-APPEND-C ( n ptr u8 n ptr len -- ), which 5 files depend on. Add packaged STR:BUF-APPEND-C ( n ptr u8 CAD-NUM:byte-len ptr len -- ) to lib/string.f mirroring STR:BUF-APPEND: bind RAW-BUF-APPEND-C to the global before the package word shadows it, take cap as CAD-NUM:byte-len via BYTE-LEN>N, keep the same c<0/c>255 E-STR-BOUNDS and E-STR-CAPACITY throws. Acceptance: STR:BUF-APPEND-C lands with tests covering append, capacity overflow throw, byte-range reject; lib/string-test.f green + manifest row. Files: lib/string.f, lib/string-test.f, lib/std.manifest. Verify: string-test, lint-manifest. Ownership: lib/string.f owner surface.
