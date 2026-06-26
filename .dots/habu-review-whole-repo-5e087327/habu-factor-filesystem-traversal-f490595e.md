---
title: Factor filesystem traversal
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-25T12:19:43.541753+02:00\""
closed-at: "2026-06-25T21:11:00.568244+02:00"
close-reason: "Factored shared traversal mechanics into lib/fs.f: self-entry skip, walk-dir open/close, dir block/record load/advance, child path enter/leave. WALK-FILES keeps repo-metadata skip policy; REMOVE-TREE keeps symlink-first deletion and descends into .dots. Validated lib/fs-test.f, lib/fs-mutate-test.f, run-attempts focused/checker-safe fixtures, stdlib-manifest-test, filemap-lint, test/gate-stdlib.f, and full native gate."
---

Finding F11. Evidence: docs/factorization-review.md:39; lib/fs.f:396 and lib/fs-mutate.f:97. Root cause: walk and remove-tree duplicate directory traversal mechanics, including open/read/filter/join/recurse/close. Fix: factor directory iteration, child path enter/leave, and close handling into lib/fs.f while keeping deletion policy in lib/fs-mutate.f. Why: traversal bugs are easy to copy into destructive paths. Validate with fs and fs-mutate tests plus full native gate.
