---
title: Read a stored zip member in pieces of at most INT_MAX bytes
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T07:20:04.336961+02:00"
---

Problem: for a stored member read from the snapshot, src/lib/zip.f INPUT-BYTES hands the whole remaining length to one C-PREAD, and for an OPEN-SOURCE input the caller's whole span to one read; in ZIP:READ that length is the whole member. macOS's man 2 pread says read and pread fail when nbyte exceeds INT_MAX, so a valid stored member over 2 GiB, or a source read into a span that large, would fail (E-FS-IO once habu-tell-zip-s-1d8de532 lands). Read from the man page and the code by the review of that dot (2026-11-11); not reproduced. Acceptance: first show the failure (a read or pread with a count above INT_MAX on a small file answers -1), then cap each read and pread at INT_MAX; READ-INNER's callers already loop over short counts. Files: src/lib/zip.f. Verify: the reproducer answers a count, and the zip suites pass. Depends: habu-tell-zip-s-1d8de532 (same lines). Ownership: src/lib/zip.f. Claim: unassigned. Consumers: none meets this today (no Office package of Zen, Tender or Maki holds a 2 GiB stored member).
