---
title: Inline small colon words at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:09:22.612042+03:00"
---

Problem: tier 1 calls every colon word through bl with the stack in memory across the call: REQUIRE-BOOT-LIMIT calls REQUIRE-BOOT-OPEN? (a 15-instruction leaf) and pays the frame, the bl, and a store-then-reload of the result through x19 (measured 2026-09-16); the inlining lane habu-inline-trivial-engine-922133ca covers engine primitives only. Acceptance: tier 1 inlines a callee whose body is a leaf below a stated instruction budget (a named constant with the measured distribution behind it) and has no locals frame, quotations or control-flow that the elaborator cannot splice; recursion and words with a DOES> or deferred binding are excluded by name; the callee keeps its own record and code for callers that are not inlined; the sample word loses its bl and frame; baked code bytes and self-build CPU before and after (inlining must not grow the engine: report both); byte fixpoint; full gate green; the profiler attribution (habu-build-the-internal-4cd07a82) still names the inlined callee through its pc range or the commit says why not. Files: src/compiler/native/elaborate.f, hir-word.f, select.f, test/compiler/*.f. Verify: tools/jitdump.f on the sample; test/compiler suites (aot-mode.f prefix); tools/native-build.f fixpoint with timing; test/run.f. Depends: habu-inline-trivial-engine-922133ca lands first (same code path). Ownership: tier-1 elaboration. Claim: unassigned.
