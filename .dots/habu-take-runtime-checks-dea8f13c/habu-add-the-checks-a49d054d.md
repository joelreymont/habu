---
title: Add the --checks build option
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:51:44.249950+02:00"
---

Problem: AGENTS.md (Changes) allows code instrumented with bounds and other checks on demand. Joel named the option --checks and shelved it on 2026-11-13; meanwhile the lanes under habu-take-runtime-checks-dea8f13c delete those checks from default code and disable their tests with `off until dot` and this dot's id. Acceptance: tools/hb-build.f --checks builds an engine or a program whose code carries the deleted checks again (buffer index bounds, tag on fetch, MATCH fall-through, zero divisor, 0 throw, Wasm load address, library preconditions, task and AIO ownership, the linker's own state), restored from those commits; definers and library code test the choice through HB-CHECKS?, and T0 folds `HB-CHECKS? if ... then` so default code carries nothing; checked objects never share a directory with default ones, so a warm build still equals a cold one; the disabled tests run on a --checks engine. On Wasm run clears the fault kind, a fault a run catches leaves none recorded and a finally keeps its body's kind and address, so a later trap never names an earlier fault. Depends: none. Claim: unassigned.

Acceptance case 2026-11-13 (from the NFETCH-CHECK test cleanup): the disabled cases have no callers even inside their `false [if]` blocks: test/program-diagnostics-test.f BAD-TAGS (~102) is called by nothing, and test/file-objects.f PART-CASE (~835) is missing from the case list (~1775-1781). Turning the blocks on must also call both. The fixture test/fixtures/file-objects/part-main.f no longer holds a part: rename it for what it is, a linked program that fetches an out-of-domain tag.

Acceptance case 2026-11-13 (jerry; from habu-drop-the-zero-15fc76ff): the checker refuses a literal 0 divisor of `/`, `mod` or `/mod` (E-DIV-ZERO) and a literal `0 throw` (E-THROW-ZERO) in every build, so --checks restores the run-time zero test for computed divisors only; a switched-off case that divides or throws by a literal 0 (test/wasm/numeric-rows.f N02-DIV-ZERO, test/wasm/hb-build.f throw-zero.f and CATCH-ZERO+, test/wasm/kernel.f ZERO-DIVISOR) passes its 0 computed when turned on.

Acceptance case 2026-11-13 (from the dot-citation recheck): test/wasm/hb-build.f THROW-ZERO-CASE (:477) and CATCH-ZERO+ (:485) also expect the code ENGINE-ERROR:THROW-ZERO (109), which no longer exists, and nothing calls either.
