---
title: "End a file's scratch state and tasks with the file"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:18:44.425366+02:00"
---

Problem: design habu-design-no-file-298feb05, items 2 and 3. A later file's top level sees an earlier file's scratch cells as that file left them in a cold build and as zero in a warm build where the earlier file linked; a task left running at a file's end can change data the object holds (src/habu/linker.f PIECE-END). Acceptance: PIECE-END zeroes every scratch cell in file data [OPEN-DP, DP) after the piece's stores are taken, and refuses a piece that ends with hb's TASKS-LIVE-CELL above its value at the open: one line 'linker: <file>: a task is still running at the file's end', then LINKER:E-LIVE-TASK (-9694). test/file-objects.f: a later file's top level reads 0 from a scratch cell an earlier file's top level stored 7 into; a build of a file that leaves a task running fails with that line, and fails again on the next build. Known: ffi-callback goes red through IMAGE-LIFECYCLE's count until the task.f and image-lifecycle.f owners change, so this lands with them. Files: src/habu/linker.f, test/file-objects.f, test/fixtures/file-objects/. Verify: engine generations equal; test/file-objects.f. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-piece.
