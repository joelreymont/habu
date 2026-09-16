---
title: WALK-FILES bakes in repo policy and aborts on dangling symlinks
status: closed
priority: 2
issue-type: task
created-at: "\"2026-08-22T22:38:25.954142+02:00\""
closed-at: "2026-09-16T14:34:50.118647+03:00"
close-reason: "superseded by habu-campaign-c5-runtime-723833a3: Residue: the file walk bakes repo policy into the library, applies it to the root, and aborts the whole walk on one dangling link"
---

Problem: lib/fs.f:342-347 hardcodes '.jj .jj-ws .git .dots' as skipped names and :461 applies it to the root (a walk rooted at .git does nothing); :462-463 uses stat for both FILE? and DIR? so one dangling link anywhere throws E-FS-STAT out of the whole walk and symlink loops recurse to E-FS-DEPTH; 19 tools/tests consume it with no opt-out. Acceptance: the skip predicate is a quotation parameter (a default lives with the callers that want repo policy); lstat decides descent; tests with a dangling link and a loop. Files: lib/fs.f, callers. Verify: fs tests. Depends: none. Ownership: fs. Claim: unassigned.
