---
title: Qualify the five compiler-host products
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:29.024631+03:00"
---

Problem: five host profiles are declared (src/compiler/target/model.f:9-17: aarch64-apple-darwin, aarch64-unknown-linux-gnu, x86-64-unknown-linux-gnu, aarch64-pc-windows-msvc, x86-64-pc-windows-msvc; the two Wasm profiles are targets, not compiler hosts), and no document says which of them is a qualified compiler host. Two can be qualified now: macOS arm64 and Linux arm64 (spark). Linux x86-64 builds are off until x86-64 moves onto the linker (docs/xcomp.md:157, docs/bootstrap.md:22). The Windows profiles are declarable only, with no process or image services (src/compiler/target/model.f:127, docs/xcomp.md:43-44), and stay parked under habu-specify-windows-x64-3cd4cef3. The original also asks for cold-prefix behaviour and source-free startup (the engine starting with no source tree to read); no test or doc on master names either as a check. This dot does not authorize another platform implementation to fill the list, and no historical bootstrap graph, receipt framework or host-service record is a step of it.

Acceptance: docs/xcomp.md "Target contract" states, for each of the five host profiles, either qualified, with the master commit, the candidate engine and its `.map` built from a private copy of that tree (docs/gate.md "Qualifying a candidate") and the result of each check run on that machine: the engine build (`tools/hb-build.f -- --engine`), the full suite (`test/run.f`) on the candidate, the convergence check (docs/bootstrap.md "Convergence check"), and the test that covers cold-prefix behaviour and source-free startup or the statement that none exists; or not qualified, with the reason. Results are recorded as they came out, failures included.

Files: docs/xcomp.md.

Verify: on this Mac and on spark, the commands docs/gate.md "Qualifying a candidate" and docs/bootstrap.md "Convergence check" name, run once each from the recorded commit; the Target contract section names all five profiles.

Depends: none.

Ownership: unassigned. Claim: unassigned.
