---
title: "Inline STR:DATA, STR:LEN and >str in T0"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T17:56:41.941752+01:00"
---

Problem: STR:DATA and STR:LEN compile to calls (Joel 2026-11-23: inlined into the correct and tight assembler); .out/designs/design-str.md decision 3: OPS-MAP and INLINE gain >str (nothing), DATA (drop the top cell), LEN (nip); CALL-TOKEN inlines a record whose code start is an inlined primitive's; DO-LOCAL loads the one cell DATA or LEN reads from a two-cell local. Acceptance: `s STR:LEN` on a str local emits the same instructions as a split local's `u`, and `( str -- ^u8 ) STR:DATA` emits no call, on arm64, x86-64 and Wasm. Depends: none. Claim: unassigned.
