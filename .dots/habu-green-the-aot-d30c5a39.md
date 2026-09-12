---
title: Green the AOT gate family
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T10:57:56.504008+03:00"
---

Problem: on the complete run of 2026-09-12 (engine 04701ef9, LESSONS.md red set) the AOT and build-gate suites are red, exit code and first failure: native-gate-aot-positive (1: its own pool reports fork hb-build AOT preseed rc 74 and fork hb-build AOT bundle/data rc 74); native-gate-diagnostics (67: uncaught 7143); check-cli-boundary (67: uncaught 7143); hb-build-fixtures (67: uncaught -2504); app-image (67: F4, uncaught -2500 E-PROC-SPAWN); aot-wid-restore (70: E-UNDEFINED: PROT-WID-LEGACY-MAX); aot-wide-format (67: F1); aot-sig-pool (1: F21); aot-prelude-band (1: expected range 26689544); program-diagnostics (1: FAIL test/aot-address-cell-lower-straddle-bad.f). native-gate-aot-negative is red for the strict parametric rule (habu-integrate-strict-parametric-c39bbc70). The 1.7 s campaign's evidence stands on native-gate-aot-positive and negative being green, so this cause goes first. Acceptance: each suite green on the root engine, or its case retired with the reason in the commit and the LESSONS.md red set updated in the same commit; one commit per distinct defect. Files: test/gate-aot-positive.f and -lib.f, test/gate-diagnostics.f, test/check-cli-boundary.f, tools/hb-build*.f, test/app-image*.f, the aot-* suite files named in test/gate-stdlib-cases.f. Verify: each suite standalone on the root engine, then test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
