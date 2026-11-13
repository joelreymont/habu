---
title: "Bound a Wasm run's host records by its operations, not its meshes"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T13:11:21.471061+02:00"
---

Problem: a Wasm run writes one 32-byte host record per mesh it adds (turn.js record 13) or drops (record 14) into OUT, WPROF:OUT-CAP 64 KiB (src/arch/wasm/profile.f:33), so Maki's viewer traps at about 2041 meshes in a turn; the trap is emit's bare unreachable (src/arch/wasm/link-routines.f:132), fault kind 0 and throw code 0, so the host cannot name a full OUT. Request: /tmp/bob-habu-wasm-records.md. Acceptance: a turn that adds or drops any number of meshes runs; a full OUT reaches the host as a named fault; Maki's reproducer (BLOCKS=2041 and up) passes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wasmrec
