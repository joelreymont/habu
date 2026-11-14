---
title: Take a C int input at its width
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T00:42:24.721469+01:00"
---

Problem: FUNCTION: has no input type for a C int: i32 is an output only (src/lib/ffi-abi.f OUT-TOKEN), and the header's example declares getsockname's int sockfd as n (:919). On macOS a fixed input past x7 is packed at its C width, so an int declared n takes 8 bytes and shifts every later stack input: for long f(long x8, int a, int b) Apple reads b at offset 4 and Habu stages it at 8. Acceptance: an i32 input loads with C width 4, and a C function taking two ints past x7 returns C's answer on Linux arm64 (test/ffi-c-calls.f). Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-i32.
