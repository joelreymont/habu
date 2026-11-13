---
title: "Move PIPE-PAIR's callers onto FD:PIPE and remove PIPE-PAIR"
status: open
priority: 3
issue-type: task
created-at: "2026-11-05T11:51:52.705578+02:00"
---

Problem: src/lib/process.f:304 PIPE-PAIR makes a pipe whose ends reach every program any task execs; Joel 2026-11-05 chose one pipe word, FD:PIPE (fd.f), whose ends close on exec. 83 PIPE-PAIR uses remain in 24 files, nearly all tests: process-test 11, fd-test 10, tools/lsp-test-lib 9, spawn-test 9, test/gate-pool-orphan-test 5, process-fork-test, process-argv-test, content-length-test 4 each, and 16 files with 1-3. Acceptance: every caller uses FD:PIPE; an end a child must hold is named in the child's PROCESS map (dup2 clears close-on-exec), never inherited by accident; PIPE-PAIR and its E-PROC-OUTPUT refusal path go. Each changed test passes on macOS and spark. Lanes: a few named files per brief, one commit each, no directory sweep; the last lane removes PIPE-PAIR. Files: the 24 callers, src/lib/process.f. Verify: each changed test file. Depends: none. Ownership: carl. Claim: unassigned.
