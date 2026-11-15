---
title: Re-pin the macOS FFI struct tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T07:05:38.776360+01:00"
---

Problem: test/ffi-struct-arg.f:182-210 and test/ffi-struct-result.f:143-152 expect the bare E-FFI-SYNTAX code a bad clause threw, test/ffi-struct-arg.f:239 the E-FFI-ARITY PLAN-STRUCT threw, and test/ffi-struct-verify.f compares verifier packets; src/lib/ffi-test.f:838-842 loads them on macOS only, so no Linux run shows them. Acceptance: ffi-cabi passes on a macOS arm64 engine of the C4 head. Depends: none. Claim: unassigned.
