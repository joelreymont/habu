---
title: compiler suites test the baked chain, not the source
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T12:51:39.090183+02:00"
---

Problem: bin/hb carries the native compiler as a baked AOT payload, so 'require src/compiler/native/*.f' is a no-op in a test and test/compiler/native-*.f exercise the BAKED chain until 'bin/hb --load tools/build-fixpoint-refresh.f -- install' runs (measured 2026-08-23 by the RECURSE lane: appending an undefined word to src/compiler/native/elaborate.f leaves native-chain green, while the same edit to src/core/util.f fails the load rc 70). A compiler source change can therefore pass every compiler suite without ever being compiled. The gate's candidate build (phase 15) rebuilds the engine, so the full test/run.f is not fooled - but every focused run is, and the commit gate's 'behaviour suites the change touched' clause names focused runs. Acceptance: a structural guard - the compiler suites refuse to run (named message) when the baked chain's source digest differs from the checkout's src/compiler tree (the chain capture already records an artifact digest: tools/aot-chain-capture.f), or the suite entry performs the refresh itself; a fixture that edits a compiler source and shows the suite refusing rather than passing; docs/forth.md commit gate names the refresh for diffs under src/compiler. Files: test/compiler/*.f entry, tools/aot-chain-capture.f, docs/forth.md. Verify: the fixture. Depends: none. Ownership: native chain tests. Claim: unassigned.
