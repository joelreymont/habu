---
title: "Place a struct call's inputs by the call rule"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T23:57:06.697829+01:00"
---

Problem: a FUNCTION: call with a STRUCT-ARG or STRUCT-RESULT clause is planned by src/lib/ffi-abi.f PLACE-ARG (:1468), not by the call rule PLACE-NEXT applies: it puts every float in a register and rejects a ninth (FLOAT-REGS-FULL :1391, from PLAN-ROOM :1499), a limit of Habu's own, and stages every integer input past x7 as a whole cell, so on macOS a u8, u16, u32 or char there is mis-staged with no rejection; the header's 'cell-wide C types' rule (:982) is checked nowhere. Acceptance: a struct call takes floats and narrow integers past the registers by the call rule, and C functions taking them beside a record return C's answer on Linux arm64 (test/ffi-struct-arg.f); tools/check.f plans the same layout; FLOAT-REGS-FULL is gone. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-struct.
