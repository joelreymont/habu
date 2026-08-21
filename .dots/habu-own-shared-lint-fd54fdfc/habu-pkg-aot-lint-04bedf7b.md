---
title: Package AOT lint core
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:51.543853+02:00\""
closed-at: "2026-07-23T03:18:14.974865+02:00"
close-reason: Landed package AOT lint core at 7c59207d; verified in master@origin 2db115be.
---

Why: tools/aot-lint-core.f still publishes 41 package-less definitions. A representative changed body fails package-diff-lint with E-PACKAGE-OWNERSHIP, and the landed command, test, and direct-build packages now provide the complete caller boundary.

Files: tools/aot-lint-core.f, tools/aot-lint.f, tools/aot-lint-test-lib.f, and tools/hb-build-direct-lints.f only. Put the core in package AOT-LINT. Publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), JSON! ( bool -- ), OUT-FD! ( fd -- ), and FINISH ( -- ). Remove AL- from every private core identifier; use BAD-N, SCAN-I, EXPECT-NAME, CUR-A, and CUR-U for the ambiguous private state. Qualify only AOT calls in the three consumers. Leave signature-lint and source-lexer calls unchanged.

Acceptance: no AL-* implementation definition or storage remains global; no summary, count, buffer, raw cell, alias, forwarding shim, new require, or behavior change is introduced. Clean input, real rejection, JSON, source labels, output routing, and structured process outcomes remain exact. Removing the package opener or retaining any former global must fail the exact package gate.

Verify: bin/hb --load tools/aot-lint-test.f; bin/hb --load tools/hb-build-direct-lints-test.f; bin/hb --load tools/hb-build-test.f for the child CLI path; exact-diff typed-local and package lints; hostile package-opener removal; host-lint; filemap-lint.

Claim: agent=aot_core_pkg workspace=.jj-ws/habu-pkg-aot-lint-04bedf7b.
