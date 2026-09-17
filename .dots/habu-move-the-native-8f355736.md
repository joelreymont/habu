---
title: Move the native code walker out of src
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T12:57:24.826064+03:00"
---

Problem: src/compiler/native/codewalk.f (package NWALK, 72 lines) is required by nothing under src, lib or tools; only test/compiler/native-exec.f and test/compiler/native-quot.f require it, so it is test tooling shipped as compiler source and it rides along in every closure walk of src/compiler/native. The retired native-unguard lane (commit a25b0314 landed; its working copy, discarded 2026-09-17, deleted the file and reworked test/type-layout-lower-pending.f, p2-map-rewind.f, match-factor-pin.f, lit-emit-size-test.f, addrmap-call.f, aot-seeded-address-sites.f against it). Acceptance: the walker lives under test/ (or lib/test/) with its package, the two suites require it from there, nothing under src references it, and the native suites pass. Files: src/compiler/native/codewalk.f, test/compiler/native-exec.f, test/compiler/native-quot.f. Verify: the two suites; test/run.f. Depends: none. Ownership: native compiler tests. Claim: unassigned.
