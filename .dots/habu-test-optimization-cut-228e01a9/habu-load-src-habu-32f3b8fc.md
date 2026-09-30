---
title: Load src/habu/snap.f or retire it
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T23:40:05.578785+02:00\""
---

Problem: nothing loads src/habu/snap.f (rg finds only tools/lint/shadow-lint.f:228's lint row, comments at tools/aot-build-core.f:64, lib/fs-mutate.f:553, src/habu/snap-lib.f:456, test/stripped-lifecycle-running-subject.f:4, and the docs/debugging.md:221 recipe that edits it), so its RETIRE-AND-PERSIST is unreachable, and after change rlsovvvt (SNAP:PERSIST refuses an unnamed output) it would stop with 'snap: persist has no output path'. The debugging recipe was already stale. Found by the --repl reproducibility lane. Acceptance: either a real load path of snap.f is found and a test runs it, or snap.f is deleted with every reference (the lint row, the comments, the recipe rewritten against the code that now retires and persists a snapshot build, checked by running it once). Files: src/habu/snap.f, the referencing files, docs/debugging.md. Verify: shadow-lint, the stripped-lifecycle rows, the recipe run once. Depends: habu-make-two-repl-bde9f316. Ownership: snap.f's existence. Claim: agent=kestrel workspace=.jj-ws/r4-repl.
