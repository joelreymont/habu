---
title: "Spill a macOS variadic call's fixed inputs past x7"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T23:36:25.876217+01:00"
---

Problem: on macOS a variadic FUNCTION: with more than 8 fixed integer inputs is rejected (src/lib/ffi-abi.f PLAN-SPILLS REG-ROOM), though Apple arm64 passes fixed inputs past the registers on the stack before the variadic ones. Acceptance: such a call works on macOS, with a test run there. Depends: none. Claim: unassigned.
