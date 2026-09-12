---
title: Give PROC-CMD a working-directory variant
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T05:12:14.280279+03:00"
---

Problem: lib/process-command.f PROC-CMD runs the supervised child in the loader's working directory, so a gate cannot run a relocated executable from a private directory away from every source tree; Tender's Habu standalone gate needs that to prove the executable does not fall back to require lib/... or rules/... relative to cwd (the old Python gate ran with cwd=<private dir>). Reported by tender-b3 on 2026-09-12 (message 20260912-021128.347-tender-b3-8190). Acceptance: a PROC-CMD variant that takes a working directory and changes into it in the child before exec (the Linux spawn path already chdirs for spawn-io's file actions; the Darwin path folds a chdir file action), fail-closed on a missing or non-directory path with a named error, the existing PROC-CMD unchanged; a regression that runs a child from a private temporary directory and reads its cwd back. Files: lib/process-command.f, lib/process.f (the spawn seam), lib/process-command-test.f, docs/process-pty.md. Verify: the process-command suite plus the new case on bin/hb. Depends: none. Ownership: hazel (lib/). Claim: unassigned.

Result 2026-09-12 (hazel): PROC-CMD:CWD! ( ptr u8 len -- ) stores the child's working directory (refused before any spawn when missing, empty, over the path capacity or not a directory: E-PROC-PATH; RESET clears it); RUN-OUTCOME then runs through PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME, the outcome-returning twin of the existing cwd capture over the spawn-argv-env-cwd-io primitive, and PROC-CMD itself is otherwise unchanged. lib/process-command-test.f gains a /bin/cat here.txt run from a fresh TMPDIR-MKDIR directory holding that file (a relative-path proof, immune to symlinked or slash-ended TMPDIR), and a missing-directory CWD! that throws E-PROC-PATH; against the unchanged library the suite dies E-UNDEFINED. process-command and process-cwd suites pass on the root engine.
