---
title: Trim FFI after its run-time checks went
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:31.644291+01:00"
---

Problem: the feature review of FFI's run-time checks out (master..mlsvtpvl, carl-band) left four leftovers in src/lib/ffi-abi.f and check.f. (1) OUT@, OUT!, LIBRARY-NAME$ and LIBRARY-PATH-CAP are public, and outside ffi-abi.f only src/lib/ffi-test.f calls them. (2) tools' declarer check keeps its own 16 (src/habu/verify-source.f:3386 FFI-IN-CAP) beside FFI's ARG-MAX, so a raised ARG-MAX makes check and load disagree. (3) FFI-BUF is 16 cells (src/habu/layout.f FFI-ABI BUF-BYTES) while no row loads past x8, so cells 9-15 are never used. (4) The private VALUE!, POINTER!, FLOAT!, STACK-VALUE!, STACK-U8!/U16!/U32! and STACK-FLOAT! only forward to FFI-ARG!, FFI-PTR-ARG!, FFI-FARG! and FFI-STACK!. Acceptance: no public FFI word only tests call (deleted, or private with its test reaching it through FUNCTION:); check.f reads FFI's bound instead of a copy; the band holds the cells the rows use; one name per staging store. Depends: the landing of carl-band. Claim: unassigned.
