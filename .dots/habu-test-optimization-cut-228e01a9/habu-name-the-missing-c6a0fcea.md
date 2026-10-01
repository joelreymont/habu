---
title: "Name the missing member in BUILD-WITH's reader"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T10:49:31.106044+02:00"
---

Problem (r4-repl lane, dot 808da670 follow-up): BUILD-WITH's member reader READ-COLLECT (tools/native-source-view.f:153) calls READ-ALL (lib/fs.f:482), which throws E-FS-OPEN without naming the path, so a missing member ends as a bare throw code that does not say which file. EC:BUILD's reader now names it (discover: cannot read <path>, then the same code). Acceptance: a missing BUILD-WITH member names its path on fd 2 and keeps its throw code, through the same rule EC uses; a case through the real load path, written first and seen to fail. Files: tools/native-source-view.f, its test. Verify: the case, the native-source-view and native-build tests that cover BUILD-WITH. Ownership: BUILD-WITH member read.
