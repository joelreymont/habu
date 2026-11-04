---
title: "Move FFI-DECL's INT-N and FLT-N off static DATA"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.880056+01:00"
closed-at: "2026-10-31T11:07:54.130226+02:00"
close-reason: "Landed 3847e27a 'Plan FFI registers on the stack': INT-N, FLT-N, CALL-KIND and STACK-N leave static DATA; planning counts travel on the stack; ARG-REG's CONTEXT-CELLS row goes. Proof: candidate built by the installed hb rc 0; ffi-abi-test, ffi-test, ffi-growth, ffi-callback-growth rc 0; full registry 510/510 rc 0 (668 s). x86-64 SysV and Linux arm64 plans rest on review by reading."
---

src/lib/ffi-abi.f FFI-DECL keeps INT-N/FLT-N as static variables (around :1191). Rule: static DATA only for state that outlives a call; builder tool DATA is captured into app images, so leftover scratch changes a hit's image. Judge whether they outlive a call; if not, carry them on the stack or in locals. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-ffid (brief carl-dots/.out/carl-file/brief-ffid.md).
