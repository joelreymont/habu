---
title: Keep the DATA base from programs on every engine
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T21:52:26.059700+01:00"
closed-at: "2026-11-19T11:32:17.520108+01:00"
---

Problem: the unlanded data-base rows (src/habu/prims.f:734-789 in jerry-dbp, srornlkv) make each row's package in every engine, sealed and empty where the engine does not load its file before the seal. src/lib/reason.f:22 is rejected E-SEALED-PACKAGE in Wasm builds (wasm-device, Maki's viewer), and Tender's package NATIVE (input-view.f:7) natively. Settled by the long-term rule: data-base keeps one private row, BOOT's; each calling engine package imports it with `EXPORT BOOT:data-base`, which EXPORT takes once it accepts a private primitive of a package that is not sealed; reason.f, byte-buffer.f and data-floor.f load before the Wasm seal as they do natively; the checker-only DATA-OFF and STRING-ABI's SB offsets become private. Acceptance: one data-base row; every Habu file a program loads today loads and runs on each target that loads it (wasm-device, maki-viewer, tender-server pass); test/x86-64-kernel-prof.f's report case checks the image's reports without the host's profiler cells. Depends: none. Claim: agent=jerry workspaces=.jj-ws.noindex/jerry-dbp, jerry-ex (EXPORT), jerry-xp (x86 report case).
