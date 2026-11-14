---
title: "Run the FFI C-call tests on macOS"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T23:36:25.876217+01:00"
---

Problem: the Apple arm64 stack layout of FUNCTION: calls (src/lib/ffi-abi.f SLOT-BYTES, SPILL-ARG and PLACE-NEXT: a fixed input past the registers packed at its C width, every variadic input in an 8-byte slot) is checked only by reading; no macOS host has run test/ffi-c-calls.f, whose callees hb_cc_f9, hb_cc_l8n, hb_cc_l9v and hb_cc_l9vu pin it. Acceptance: src/lib/ffi-test.f, which loads test/ffi-c-calls.f and the struct tests (hb_nstack packs its narrow inputs there), passes on macOS arm64. Depends: none. Claim: unassigned.
