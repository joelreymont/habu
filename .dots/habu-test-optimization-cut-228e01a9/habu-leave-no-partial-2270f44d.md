---
title: Leave no partial image when a snap write fails
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T17:17:49.821777+02:00"
---

Problem: src/habu/snap-lib.f WRITE-BYTES opens the target path itself (:505, O_WRONLY|O_CREAT|O_TRUNC, mode 0755) and dies 74 on a failed write or close (:526), leaving a truncated, unsigned executable at the output path (review 172; test/snapshot-writer-close-fail.f leaves one). A later run can execute it. Acceptance: the image is written to a sibling path in the same directory and renamed over the target only after write, close and signing succeed; any failure removes the sibling and leaves the target as it was (absent or the previous file); the close-fail case shows a leftover image before the change and none after. Files: src/habu/snap-lib.f, test/snapshot-writer.f. Verify: test/snapshot-writer.f rc 0; app-image builds unchanged.
