---
title: "Drop a definer's dead parent from a native stripped program"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:36:01.328699+01:00"
---

Problem: src/habu/link-band.f PLACE-STEP (:385-388) places a reached emission whole and resolves every site in it, dead parts too, so a native stripped program that reaches only a does> child's clause keeps its definer's parent unit and what that calls: 17,512 bytes of tenderd on arm64, 14,498 on x86, most through BEGIN-STRUCTURE (structures.f:77). The Wasm link writes such a function as unreachable. Acceptance: on arm64 and x86-64 as on Wasm, a function no live record's range meets is not linked with its calls, and a stripped program reaching a child's clause holds no create or does-patch code. Depends: none. Claim: unassigned.
