---
title: Make the engine build independent of its driver
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T18:54:49.899009+03:00"
---

Problem: two runs of tools/native-build.f are byte-identical, but adding a single `require lib/fs.f` to the driver changes the built engine at byte 143718 (a constant in (LP2VEXEC)), so tools/build-profile.f, which requires the profiler around the same driver, builds a valid engine that is not byte-identical to the shipping one (profiler lane, 2026-09-16). An engine's bytes should be a function of the source tree and the host engine, never of which tool invoked the build. Acceptance: find what the driver's own require closure leaks into the image (the constant, its owner, and why the driver's loaded state is visible to capture), make capture independent of it (or refuse a build whose driver state would differ from the shipping driver, by name), and prove it: an engine built by tools/build-profile.f equals one built by tools/native-build.f byte for byte; tools/two-generation-build.f and the fixpoint unaffected. Files: src/habu/aot-capture.f, habu2.f, tools/native-build*.f, tools/build-profile.f, test/. Verify: the equality; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: capture. Claim: unassigned.
