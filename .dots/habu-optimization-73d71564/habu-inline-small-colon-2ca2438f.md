---
title: Inline small colon words at tier 1
status: closed
priority: 2
issue-type: task
created-at: "2026-09-16T16:09:22.612042+03:00"
closed-at: "2026-09-30T17:20:00.000000+02:00"
close-reason: "Dropped. The case rests on one sampled call (REQUIRE-BOOT-LIMIT to a 15-instruction leaf) and no measured speed gain; the acceptance forbids growing the engine, and two of five steps (f96ea1d6 and a99b02d0, not landed) added 7.8 KB of compiler code with nothing spliced. Its only consumer was the elision slice of habu-remove-bounds-checks-22af10b0. docs/engine-size.md, 'ARM64 code-generation changes that were removed'."
---

Problem: tier 1 calls every colon word through bl with the stack in memory across the call: REQUIRE-BOOT-LIMIT calls REQUIRE-BOOT-OPEN? (a 15-instruction leaf) and pays the frame, the bl, and a store-then-reload of the result through x19 (measured 2026-09-16); the inlining lane habu-inline-trivial-engine-922133ca covers engine primitives only. Acceptance: tier 1 inlines a callee whose body is a leaf below a stated instruction budget (a named constant with the measured distribution behind it) and has no locals frame, quotations or control-flow that the elaborator cannot splice; recursion and words with a DOES> or deferred binding are excluded by name; the callee keeps its own record and code for callers that are not inlined; the sample word loses its bl and frame; baked code bytes and self-build CPU before and after (inlining must not grow the engine: report both); byte fixpoint; full gate green; the profiler attribution (habu-build-the-internal-4cd07a82) still names the inlined callee through its pc range or the commit says why not. Files: src/compiler/native/elaborate.f, hir-word.f, select.f, test/compiler/*.f. Verify: tools/jitdump.f on the sample; test/compiler suites (aot-mode.f prefix); tools/native-build.f fixpoint with timing; test/run.f. Depends: habu-inline-trivial-engine-922133ca lands first (same code path). Ownership: tier-1 elaboration. Claim: unassigned.

Reading (heron, 2026-09-30): "leaf" means token-leaf, a body with no returning call to a colon word; engine-primitive and guarded dialect lowerings are admitted. Locals are admitted by renaming them per site. The strict code-leaf and no-locals reading leaves lib/span.f AT and U8! ineligible, so habu-remove-bounds-checks-22af10b0 could never be reached. Design: ~/.cache/tmp/heron-arm64/design-inline.md (splice the callee's retained expanded tape into the caller's tape before elaboration; session-owned retention; the budget constant is chosen by measurement).
