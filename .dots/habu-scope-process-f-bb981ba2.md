---
title: "Scope process.f's capture words in a package"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:22:13.779260+02:00"
blocks:
  - habu-stream-a-cmd-a9e146a3
---

Problem: package PROCESS (src/lib/process.f) exposes a capture's state as public cells, PROCESS:PID, OUT-W, ERR-W, IN-R and the rest (src/lib/process.f:108-121), so code outside the process library adopts pids and moves descriptors by writing them directly instead of through PROCESS:CAPTURE-PID!. test/gate-common-lib.f:90-92 spawns with PROCESS:OUT-W and ERR-W and stores the pid with `PROCESS:PID !` (again at :399); src/lib/test/subject.f:43-46 dups and closes OUT-W and ERR-W; test/capture-tree-test.f, test/process-stdin-zero-child.f and src/lib/test/runner.f also reach the cells. A review found two adopters bypassing PROCESS:CAPTURE-PID! this way. "Put process.f in packages" (5c6990a57c) renamed the old PROC-* globals but kept the cells public. Which public words a capture package offers (adopt a pid, read exit state, end a capture, hand a child its output descriptors) and their names are Joel's to choose.
Acceptance: a design listing the capture package's public words and names, approved by Joel and recorded here; then the cells are private, and every adopter outside src/lib/process*.f goes through those words.
Files: src/lib/process.f, src/lib/process-argv.f, src/lib/process-env.f, src/lib/process-cwd.f, src/lib/process-fork.f, test/gate-common-lib.f, src/lib/test/subject.f and the other adopters named above.
Verify: rg -n 'PROCESS:(PID|RC|OUT-R|OUT-W|ERR-R|ERR-W|IN-R|IN-W|OUT-LEN|ERR-LEN|IN-OFF)\b' outside src/lib/process*.f prints nothing; hb --load src/lib/process-test.f, src/lib/process-env-test.f and test/capture-tree-test.f each rc 0.
Depends: habu-end-a-capture-ce0c5a71 (timer lane 1, which reworks capture ending, lands first).
Ownership: unassigned. Claim: unassigned.
