---
title: source-shape substring tests gate merges
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.048327+02:00"
---

Problem: lib/test/src-shape.f:62-86 HAS?/COUNT/MUST-HAVE/MUST-LACK/COUNT= assert over a source file's bytes; consumers: tools/spawn-emitter-test.f (asserts habu1.f contains ': SPAWN-DUP2-ACTION ( reg fd -- )' and SPAWN-DARWIN-FINISH exactly 5 times), c-call-emitter-test.f, signature-scan-emitter-test.f, compiler-dispatch-test.f, codegen-role-test.f, test/underdepth-gate.f, lit-emit-size-test.f, aot-chain-capture-suite.f, candidate-validation-test.f; four of them are in tail-fast (test/gate-stdlib-inline-lib.f:385-395) and gate merges; test/boot-pin-test.f:34-37 counts textual PFX-LOAD-ROW occurrences (54). AGENTS.md: a parser/lint test must check structure with fixtures built to fool it. Acceptance: each converted to lex through tools/lint/source-lex.f and assert definitions and call sites structurally; SHAPE:COUNT= deleted; a comment carrying the old substring no longer satisfies any of them (fixture). Files: lib/test/src-shape.f and the ten consumers. Verify: tail-fast green; the fooling fixture red. Depends: none. Ownership: test harness. Claim: unassigned.
