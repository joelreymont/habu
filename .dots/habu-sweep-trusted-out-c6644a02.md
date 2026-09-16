---
title: "Sweep TRUSTED: out of the test tree"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T15:49:57.938352+03:00\""
---

Problem: 919 TRUSTED: definition sites sit under test/ (type-family-suite.f 100, engine-suite.f 97, type-decl-suite.f 64, checker-scan-index-suite.f 56, type-ctor-suite.f 49, enum-decl-suite.f 45, ...), most as fixture metaprogramming that builds unchecked helper words. They keep the definer alive and teach agents the wrong idiom. Acceptance: every test builds its helpers as checked definitions or as package-owned PRIM axioms declared in the test (the spelling src/core/checker.f uses for run-in-stack); tests that ASSERT the TRUSTED: mechanism itself are rewritten to assert the owner-bound capability or deleted with the reason in the commit; `rg -c "^\\s*TRUSTED:" test/` = 0; full gate green with the same or higher case count per suite (report per suite). Work in slices by suite family, one commit each. Files: test/**/*.f. Verify: rg count; bin/hb --load test/run.f. Depends: none for a body that calls no trust-boundary primitive (the majority; start there). A body that calls a PE-TRUSTED-ONLY primitive (checker.f 6925-6945 code-injection prims, ffi-call-bounded) waits for the mechanism decision recorded in habu-delete-the-trusted-42b30edd: hazel recommends dropping the bit and making those primitives private to their owning packages, so package privacy, not a checker flag, bounds who may call them; the FFI consumers go through aspen FFI declarer. Ownership: test/. Claim: agent=hazel-sweep-tests workspace=.jj-ws/hazel-sweep-tests. Parent: habu-trusted-dies-prim-4fd12d60.
Re-scoped 2026-09-16 after the first pass (4 commits, 217 sites converted, 919
to 702, every suite case count identical): the acceptance is 0 TRUSTED: sites
that a checked definition can express. The remaining 702 need engine decisions,
each its own child: 424 whitebox probes of engine-internal words (whitebox
suites on the unsealed cold engine), 99 deliberate casts (declared CAST:
form), 97 unmodeled bodies, 81 of them evaluate (typed evaluate boundary), 26
trust-boundary primitives (the private-prim capability), 40 in children that
do not load standalone, 7 deferred (test/bootstrap-wide-memory-src.f needs
the gforth path; test/aot-payload-graph-child.f needs the gate harness). Two
sites left on purpose: native-colon.f TRUSTED-ADD1 and native-create-does.f
TRUSTED-MAKE assert the definer compiles through the native compiler and move
with its deletion. PRIM: cannot be written in a test file (internal after the
seal), so rule (2) of the original contract was void.
