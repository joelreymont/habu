---
title: A failed PTY REPL line fails a test on a clock
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:23:55.121674+01:00"
---

Problem: the PTY REPL tests' STEP words (test/repl-address-cell-rollback.f:17-18, test/dictionary-zero.f:61-62, test/repl-open-definition.f:19-21, test/top-row-storage-test.f:507-509) wait only for ' ok', and src/lib/pty-harness.f WAIT-AFTER-WITHIN (:421-427) takes one marker, so a line the REPL refuses (its error, then REPL-QNL's '?', src/habu/interpreter.f:2779, :2815) waits out WAIT-BUDGET-MS and throws E-PROC-TIMEOUT, skipping the file's later cases, instead of reading red at once (test/AGENTS.md: a hang is named by its WAIT line, never ended by a clock). Found by the rollback re-review. Acceptance: a STEP whose line fails returns at the REPL's answer and fails its assertion with the answer in the buffer, within the time a passing line takes; the four files' cases still pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-ptystep.
