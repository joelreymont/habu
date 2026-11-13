---
title: Drop the clocks left that end test children
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T06:00:30.800416+02:00"
---

Problem: test/AGENTS.md says a hang is named by its WAIT line, never ended by a clock, yet these tests end a spawned child by a fixed clock, so a slow but healthy child under load fails E-PROC-TIMEOUT with its output dropped. Literal ms: tools/source-discovery-test.f, test/svg-pdf-e2e.f, zip-temp-e2e.f, catch-stale-suite.f, file-objects.f, source-root-test.f, process-command-run-child.f, c2-diagnostics.f, c2-init-accessor-refusals.f, c2-memory-scope-refusals.f, engine-stack-lifecycle-lib.f, address-cell-storage-oom.f, compiler/native-eval.f, compiler/arm64-link-data.f, compiler/arm64-place.f, compiler/arm64-link-run.f. Named constants: tools/hb-cli-contracts-test.f HCT-TIMEOUT-MS, hb-open-failure-test.f HOF-TIMEOUT-MS, imagedisasm-test.f IMDT-TIMEOUT-MS, load-argv-test.f LA-TIMEOUT-MS, lsp-test-lib.f CONVERSATION-MS and CHECKED-MS, test/include-refusal-e2e.f, seal.f, engine-stack-machine.f, xt-effect-test.f, code-reclaim.f, gate-common-lib.f, engine-error-package.f, whole-source-read-test.f, local-spelling-suite.f, empty-quotation.f, wide-store-seal.f, cast-negative-suite.f, top-row-storage-test.f, ffi-callback-fixture.f; the framework's own: src/lib/test/runner.f GT-DEFAULT-TIMEOUT-MS, subject.f STATUS-MS. Acceptance: each passes PROCESS:WAIT-FOREVER where it bounded a child, or its comment says what else the clock does; tests that assert a deadline of their own (gate-signal-test.f, check-signal-test.f, process-pty-io-smoke.f) stay. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-clk1 and carl-clk2.
