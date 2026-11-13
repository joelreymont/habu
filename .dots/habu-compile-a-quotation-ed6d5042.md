---
title: Compile a quotation that reads locals for x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T16:48:27.898112+02:00"
---

Problem: src/habu/t0-x64.f fills none of EMIT-REC-CELLS, ENV-OPEN, REC-STORE, REC-QUOT, ENV-LOAD and the x86-64 kernel's execute, catch and finally do not follow a record cell, so an x86-64 build refuses a quotation that reads locals (E-T0-UNFILLED); once the arena lands, src/core/arena.f and src/lib/arena.f hold such quotations in files every target compiles. The lead decided 2026-11-11: arm64 first, then Wasm, then x86-64, each refusing the form until its emitter and kernel carry it. Acceptance: the cases the Wasm dot names run the same on an x86-64 engine. Files: src/habu/t0-x64.f, src/habu/kernel-x64.f. Verify: test/quotation-locals.f on an x86-64 engine. Depends: the Wasm dot (habu-compile-a-quotation-82916e39); x86-64's turn in the target order. Ownership: none. Claim: unassigned.

Joel 2026-11-11: "x86 has nothing to do with the demo so stop working on it, we'll fix it later". No lane, design or status line for this dot until he says so.
