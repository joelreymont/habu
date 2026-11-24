---
title: "Make DECL-DIAG:HAS? take str"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:04:53.334877+01:00"
---

Problem: test/decl-diag-capture.f DECL-DIAG:HAS? takes ^u8 n, so ffi-abi-test.f (:64-83, :104-109), ffi-test.f (:315) and ffi-callback-test.f (:263) keep the split-str marker; its other callers are test/postpone-effect.f, structure-decl-suite.f, build-items.f, ffi-struct-arg.f, enum-decl-suite.f and structure-mutual.f. Acceptance: HAS? takes a str, every caller passes one, and the three ffi test files carry no split-str marker and pass check.f and their suites. Depends: none. Claim: unassigned.
