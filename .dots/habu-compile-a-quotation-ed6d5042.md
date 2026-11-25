---
title: Compile a quotation that reads locals for x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T16:48:27.898112+02:00"
---

Problem: src/habu/t0-x64.f fills none of EMIT-REC-CELLS, ENV-OPEN, REC-STORE, REC-QUOT, ENV-LOAD and the x86-64 kernel's execute, catch and finally do not follow a record cell, so an x86-64 build rejects a quotation that reads locals (E-T0-UNFILLED); once the arena lands, src/core/arena.f and src/lib/arena.f hold such quotations in files every target compiles. The lead decided 2026-11-11: arm64 first, then Wasm, then x86-64, each rejecting the form until its emitter and kernel carry it. Acceptance: test/quotation-locals.f runs the same on an x86-64 engine; each site whose comment says x86-64 rejects a quotation that reads a local (`rg 'x86-64 rejects a quotation'`) reads the local inside the quotation, and the global or stack passing kept for it is deleted. Files: src/habu/t0-x64.f, src/habu/kernel-x64.f. Verify: test/quotation-locals.f on an x86-64 engine. Depends: x86-64's turn in the target order. Ownership: none. Claim: unassigned.

Joel 2026-11-11: "x86 has nothing to do with the demo so stop working on it, we'll fix it later". No lane, design or status line for this dot until he says so.
