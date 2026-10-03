---
title: Keep SNAP-CLOSE-SEAM out of production loads
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-01T04:12:40.710276+02:00\""
closed-at: "2026-10-01T17:19:49.000630+02:00"
close-reason: Fixed by spptxwyq (review 172 ACCEPT)
---

Problem: SNAP-CLOSE-SEAM:INSTALL-TEST (src/habu/snap-lib.f:449-470) is a test-only seam that src/habu/snap.f undefines; with snap.f retired (dot 32f3b8fc) it stays public in every process that loads snap-lib.f, so production code can arm a close failure. Only test/snapshot-writer-close-fail.f:12 uses it. Acceptance: the seam is reachable only from the test's load path (or the test reaches the close failure another way) and a production load cannot name it, shown by a load that refuses the name; test/snapshot-writer-close-fail.f still fails a close. Files: src/habu/snap-lib.f, test/snapshot-writer-close-fail.f. Verify: that test, the snapshot-writer rows. Depends: habu-load-src-habu-32f3b8fc. Ownership: SNAP-CLOSE-SEAM.
