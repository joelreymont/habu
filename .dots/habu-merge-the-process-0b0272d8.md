---
title: Merge the process packages into PROCESS
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T19:04:35.702316+02:00"
blocks:
  - habu-fork-only-to-59d3557e
---

Joel 2026-10-28 (interview): PCAP, PROC-FORK, PROC-TREE, PROC-CWD, PROC-ENV-LIFECYCLE, PROC-ARGV-LIFECYCLE, FORK-CHILD, PROCESS-PTY and CMD become one PROCESS package (docs/forth.md: one subject, one package), hard cutover, carl integrates. After Fork only to exec, which deletes several of the words. Files: src/lib/process-fork.f is deleted, not renamed: after Fork only to exec it holds no fork; SET-PGID, KILL-GROUP and the parent-death reaper move to src/lib/process-tree.f beside KILL-TREE. Claim: unassigned.
