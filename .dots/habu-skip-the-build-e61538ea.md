---
title: Skip the build directory in bare-copy-lint
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T06:16:43.190898+03:00"
---

Problem: tools/lint/bare-copy-lint.f walks `.` with lib/fs.f's walk, whose FS-SKIP-DIR? (lib/fs.f:529-535) skips .jj, .jj-ws, .git and .dots but not build/. With HB_TMP=$PWD/build/tmp, a concurrent suite's copied tree (build/tmp/habu-native-suite-*/pool-*/native-unit-stale-*/tree/src/...) is linted and the row fails (exit 67, throw -2101); the same lint passes alone (seen in the B3 full run, build/logs/b3/run.log:708 of the carl-b3 workspace, and bare-copy-alone.log rc 0). Acceptance: the lint does not read files under the repository's build directory, whichever of the lint or its walk owns the skip; the lint's findings over the tree are unchanged. Files: tools/lint/bare-copy-lint.f (or lib/fs.f if the walk owns it). Verify: bare-copy-lint with a copied src tree present under build/tmp exits as it does without one; the lint's gate row. Depends: none. Ownership: tools/lint/bare-copy-lint.f. Claim: unassigned.
