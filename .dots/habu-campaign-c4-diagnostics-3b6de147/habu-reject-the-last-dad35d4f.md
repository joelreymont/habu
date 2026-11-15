---
title: Reject the last bare FFI library throws
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:02:37.691947+01:00"
closed-at: "2026-11-16T04:07:34.387340+01:00"
---

Problem: src/lib/ffi-abi.f >VERSION's non-number piece (`none OF E-FFI-SYNTAX throw`, ~:1099) and SELECT-LIBRARY (~:1680, `LIBRARY` with no operand or a path past LIBRARY-PATH-CAP) throw bare codes while a file loads; whether a program on a target without FFI that requires ffi-abi.f stops at the bare throw at ~:570 is unchecked. Acceptance: each prints one placed record in the FFI record form and stops with 70 under the three readers. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
