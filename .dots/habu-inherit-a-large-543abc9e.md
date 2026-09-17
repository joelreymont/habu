---
title: Inherit a large environment into a child process
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T07:50:10.682477+03:00"
---

Problem: lib/process's PROC-ENV-MAX is 256 entries and PROC-CMD:RUN-OUTCOME throws E-PROC-ENV (-2505) when the parent's environment plus the additions exceed it; a developer shell with 251 variables plus seven from a test fixture already fails, so inheriting the environment is unreliable on ordinary machines (found by the Tender scraper lane, 2026-09-17; the server now builds the child's environment explicitly, which it should for secrecy anyway). Acceptance: the inherited environment is sized from the actual envp at process start (or the table grows from lib/memory.f) with the ceiling stated in the header and the refusal naming the count and the limit; a test runs a child with 600 inherited variables; docs/stdlib.md states the rule. Files: lib/process-env.f, lib/process.f, their tests, docs/stdlib.md. Verify: the process suites, test/run.f. Depends: none. Ownership: lib/process*. Claim: unassigned.
