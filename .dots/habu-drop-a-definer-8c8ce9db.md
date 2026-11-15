---
title: "Drop a definer's dead parent from a native stripped program"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:36:01.328699+01:00"
---

Problem: src/habu/link-band.f PLACE-STEP (:365-369) places a reached emission whole and resolves every site in it, dead parts too, so a native stripped program that reaches only a does> child's clause keeps its definer's parent unit and the kernel code it calls (create, the comma, does-patch) though no live record's range holds them (hunt W69). After dot habu-hold-a-does-75ec9093 the Wasm link writes such a function as unreachable; arm64 keeps it. Acceptance: on arm64 as on Wasm, a function no live record's range meets is not linked with its calls, and a stripped program reaching a child's clause holds no create or does-patch code. Depends: habu-hold-a-does-75ec9093. Claim: unassigned.
