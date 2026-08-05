---
title: Declare the native ABI NZCV clobbered
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.546467+02:00"
---

CG-14. src/compiler/native/abi.f:97-124 declares every routine NZCV:UNTOUCHED while src/compiler/native/emit.f:1340-1494 emits CMP/CMPI/FCMP/FCMP0 without restoring flags; selection, allocation, and validation carry the field but never check it. Fix: both NABI constructors state CLOBBERED unconditionally — no consumer needs flag preservation, so build no per-routine analysis. A false signed contract is worse than no contract.

Claim: agent=reg-safety2 workspace=.jj-ws/habu-derive-reserved-registers-584f1071
