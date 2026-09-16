---
title: Store the snapshot DATA payload sparse
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.751274+03:00"
---

Problem: src/habu/snap-lib.f SND-COPY writes data-base..here verbatim and habu2.f EM-SNAPSHOT-RESTORE copies it back verbatim, so a snapshot carries every zero byte of every capacity-sized table; the stripped AOT path now stores DATA as non-zero runs (dc51c4db) and the metabuild seed already did, leaving the snapshot as the last dense heap image. Acceptance: the snapshot DATA payload uses the same run format as aot-lib.f, the restore decodes it onto the anonymous zero mapping, test/snapshot-writer.f passes and a size regression pins a million-byte hole to a few rows. Files: src/habu/snap-lib.f, src/habu/habu2.f, test/snapshot-writer.f. Verify: the fixture through bin/hb and a snapshot size before/after. Depends: none. Ownership: hazel line. Claim: unassigned.
