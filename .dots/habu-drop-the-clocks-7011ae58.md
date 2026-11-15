---
title: Drop the clocks left that end test children
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:00:30.800416+02:00"
---

Problem: test/AGENTS.md says a hang is named by its WAIT line, never ended by a clock, yet these tests end a spawned child by a fixed clock, so a slow but healthy child under load fails E-PROC-TIMEOUT with its output dropped. Named constants: tools/hb-cli-contracts-test.f HCT-TIMEOUT-MS, hb-open-failure-test.f HOF-TIMEOUT-MS, imagedisasm-test.f IMDT-TIMEOUT-MS, load-argv-test.f LA-TIMEOUT-MS, lsp-test-lib.f CONVERSATION-MS and CHECKED-MS, test/include-refusal-e2e.f, seal.f, engine-stack-machine.f, xt-effect-test.f, code-reclaim.f, gate-common-lib.f, engine-error-package.f, whole-source-read-test.f, local-spelling-suite.f, empty-quotation.f, wide-store-seal.f, cast-negative-suite.f, top-row-storage-test.f, ffi-callback-fixture.f; the framework's own: src/lib/test/runner.f GT-DEFAULT-TIMEOUT-MS, subject.f STATUS-MS; src/lib/process-command-test.f HB-TIMEOUT-MS (RUN-FAILS); test/svg-pdf-e2e.f:138 reads its ending with PROCESS:ENDING>DEADLINE-RC, which ENDING>RC replaces once no deadline is set. Acceptance: each passes PROCESS:WAIT-FOREVER where it bounded a child, or its comment says what else the clock does; tests that assert a deadline of their own (gate-signal-test.f, check-signal-test.f, process-pty-io-smoke.f) stay. Depends: none. Claim: unassigned.
