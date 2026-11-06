---
title: Write file objects without a full sync
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:36.278486+02:00"
---

Problem: L2a writes each object through FS:ATOMIC-WRITE-FILE, which syncs with F_FULLFSYNC (about 14 ms per object, about 2 s of a 157-file cold build), yet a missing, short or torn object already counts as stale and is rebuilt (design-fobj.md section 2 item 10), so the sync buys nothing. Acceptance: objects are written to a sibling and renamed into place without a sync; measured cold-build time before and after. A new FS word, if needed, goes to Joel first. Files: src/habu/linker.f, src/lib/fs.f only if a word is needed. Verify: test/file-objects.f; cold build time of its fixture and of hb-build at step 5a. Depends: L2a. Ownership: carl, worker lane. Claim: unassigned.

Open: without a sync, a crash can leave an object of the right size whose later blocks are zeros. The lane must show that such an object reads as stale (its magic, key or INPUT fail), or add a hash of the object's bytes that the reader checks; a new object field is a saved-format change and goes to Joel first.
