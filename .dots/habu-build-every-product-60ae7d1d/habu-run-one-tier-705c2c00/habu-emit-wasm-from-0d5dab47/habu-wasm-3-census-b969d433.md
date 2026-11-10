---
title: "Wasm 3: census of the kernel primitives a Wasm program reaches"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T16:58:14.892837+02:00"
closed-at: "2026-11-10T17:08:40.225965+02:00"
close-reason: "hunt done, no commit: '53 emit' reaches emit (a WKERNEL row, needs a raw routine); '2 3 + . cr' reaches . and cr; a string and loop program adds type (kernel-words.f words WKWORDS:DOT, NEWLINE, TYPE-BYTES). Each target is a dictionary record's XREF-START in hb's kernel; 1 emission placed of 12019. Design in .out/step3-design-notes.md"
---

Step 3 of the Wasm campaign. Problem: tier 0 compiles most primitives as calls to hb's kernel by host address; a Wasm module has no hb kernel, so each primitive a program reaches needs a Wasm routine, and which ones three small programs reach is not known. Acceptance: per program the kernel targets by name with site counts, the reach's counts, and for each whether src/arch/wasm/kernel.f or kernel-words.f provides it; no commit. Files: none (a hunt; probes uncommitted). Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-kernel-census.md. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3c.
