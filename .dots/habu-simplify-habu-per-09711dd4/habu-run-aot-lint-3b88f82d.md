---
title: Run aot-lint in process in hb-build
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.120834+03:00"
---

Problem: tools/hb-build-lib.f:456-504 spawns a child hb to run aot-lint, with a hand-listed 10-file closure and a 120 s timeout, although the in-process hook tools/hb-build-direct-lints.f exists and the gate already uses it. tools/cli-run.f:14-35 and 78-98 (the CLI-CHECK* family) have no callers. Acceptance: hb-build runs the lint in process; the child path and its defer deleted; CLI-CHECK* deleted; hb-build tests green. Files: tools/hb-build-lib.f, tools/hb-build-direct-lints.f, tools/cli-run.f. Verify: hb-build test rows. Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
