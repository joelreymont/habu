---
title: Give PROC-CMD a working-directory variant
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T05:12:14.280279+03:00"
---

Problem: lib/process-command.f PROC-CMD runs the supervised child in the loader's working directory, so a gate cannot run a relocated executable from a private directory away from every source tree; Tender's Habu standalone gate needs that to prove the executable does not fall back to require lib/... or rules/... relative to cwd (the old Python gate ran with cwd=<private dir>). Reported by tender-b3 on 2026-09-12 (message 20260912-021128.347-tender-b3-8190). Acceptance: a PROC-CMD variant that takes a working directory and changes into it in the child before exec (the Linux spawn path already chdirs for spawn-io's file actions; the Darwin path folds a chdir file action), fail-closed on a missing or non-directory path with a named error, the existing PROC-CMD unchanged; a regression that runs a child from a private temporary directory and reads its cwd back. Files: lib/process-command.f, lib/process.f (the spawn seam), lib/process-command-test.f, docs/process-pty.md. Verify: the process-command suite plus the new case on bin/hb. Depends: none. Ownership: hazel (lib/). Claim: unassigned.
