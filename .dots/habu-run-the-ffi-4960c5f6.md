---
title: Run the FFI struct tests on Linux arm64
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T22:47:41.930069+01:00"
---

Problem: src/lib/ffi-test.f FFI-T-STRUCT-LOAD loads test/ffi-struct-result.f, test/ffi-struct-arg.f and test/ffi-struct-verify.f only on macOS: results are called against macOS libraries (libSystem's inet_makeaddr, div and lldiv, CoreMedia's CMTime), arguments against a fixture library built for the run (STRUCT-FIXTURE:BUILD). Linux arm64, where the suite runs, never makes a struct-by-value call or meets the declaration rejections test/ffi-struct-arg.f asserts. Acceptance: all three files run in ffi-cabi on Linux arm64, results against glibc and the run's fixture library, and pass. Depends: none. Claim: unassigned.
