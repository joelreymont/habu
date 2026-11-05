---
title: "FS:ATOMIC-WRITE-FILE survives a crash"
status: open
priority: 1
issue-type: task
created-at: "2026-11-05T12:36:52.409391+02:00"
---

From kevin (Tender), 2026-11-05. Problem: src/lib/fs-mutate.f ATOMIC-IN-DIR (:653-669) fills the unique sibling, closes it and renames it over the destination with no sync of the sibling before the rename and none of the directory after (rg fsync src/lib/fs-mutate.f finds nothing), so after an OS crash or power loss the destination can exist under its final name empty or short. Tender's KEEP (tenderd/runner.f) keeps notice originals through FS:ATOMIC-WRITE-FILE where Zen's ORIGINAL:WRITE (zen src/original-store.f:53-69) synced the file and its directory; an empty original under its digest name is never fetched again. Measured 2026-11-05 (200 writes of 20 KB, scratch): macOS fsync +0.03 ms a write, macOS F_FULLFSYNC on file and directory ~9.8 ms, spark ext4 fsync ~9 ms. Design to Joel (interview): sync inside ATOMIC-WRITE-FILE and ATOMIC-WRITE-PRIVATE, or a separate synced word. Files: src/lib/fs-mutate.f, src/lib/fs-mutate-test.f. Ownership: carl. Tell kevin the word when it lands.
