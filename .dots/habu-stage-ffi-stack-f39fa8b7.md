---
title: Stage FFI stack inputs at their C width
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T23:57:06.697829+01:00"
---

Problem: src/lib/ffi-abi.f SPILL-ARG rejects an f32 past the float registers on every target (F32-UNSTAGED) and, on macOS, an integer input narrower than a cell past x7 (NARROW-SPILL), because STAGE-SPILL writes whole cells. The C ABIs give both a stack slot: AAPCS64 and SysV an 8-byte slot holding the value in its low bytes, Apple arm64 the type's own size and alignment. These are limits of Habu's own, and F32-UNSTAGED's 'has no stack slot' is false. NARROW? repeats the checker's narrow-type set by name, and the struct path (PLACE-ARG, STAGE-C-ARG) does not apply it. Acceptance: test/ffi-c-calls.f calls C functions with f32, u8, u16 and u32 inputs past the registers on Linux arm64 and gets the right results; F32-UNSTAGED, NARROW-SPILL and NARROW? are gone; the planner lays out Apple's stack by Apple's rule. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-width.
