---
title: "Name SIGNAL's stop and continue where they are spelled again"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:51:48.143457+02:00"
---

Problem: src/lib/process-tree.f:67-70 keeps a private SIGSTOP number (process.f requires process-tree.f and signal.f requires process.f, so it cannot use SIGNAL:SIGSTOP) without the comment process.f:503 gives its SIGTERM twin naming the SIGNAL word; src/lib/process-tree-test.f:25 has '19 constant SIGCONT-MACOS', which can be SIGNAL:SIGCONT once that word is on master (habu-name-sigstop-and-7b6d19c0). Acceptance: process-tree.f's constant carries a comment naming SIGNAL:SIGSTOP as the public spelling; process-tree-test.f reads SIGNAL:SIGCONT and holds no copy of the number. Files: src/lib/process-tree.f, src/lib/process-tree-test.f. Verify: bin/hb --load src/lib/process-tree-test.f. Depends: habu-name-sigstop-and-7b6d19c0 landed. Ownership: unassigned. Claim: unassigned. Consumers: met by none.
