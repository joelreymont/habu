---
title: Inherit a large environment into a child process
status: active
priority: 3
issue-type: task
created-at: "\"2026-09-17T07:50:10.682477+03:00\""
---

Problem: lib/process's PROC-ENV-MAX is 256 entries and PROC-CMD:RUN-OUTCOME throws E-PROC-ENV (-2505) when the parent's environment plus the additions exceed it; a developer shell with 251 variables plus seven from a test fixture already fails, so inheriting the environment is unreliable on ordinary machines (found by the Tender scraper lane, 2026-09-17; the server now builds the child's environment explicitly, which it should for secrecy anyway). Acceptance: the inherited environment is sized from the actual envp at process start (or the table grows from lib/memory.f) with the ceiling stated in the header and the refusal naming the count and the limit; a test runs a child with 600 inherited variables; docs/stdlib.md states the rule. Files: lib/process-env.f, lib/process.f, their tests, docs/stdlib.md. Verify: the process suites, test/run.f. Depends: none. Ownership: lib/process*. Claim: agent=hazel-env-ceiling workspace=.jj-ws/hazel-env-ceiling.

Added 2026-09-17 (whitebox lane): PROC-ENV-CHECK-EXTRA throws a bare E-PROC-ENV (-2505) with no diagnostic, so a gate whose parent shell exports 251 variables dies `uncaught throw code -2505` with nothing to act on when two suites add three of their own; acceptance also covers the refusal naming the ceiling and the count it saw.
