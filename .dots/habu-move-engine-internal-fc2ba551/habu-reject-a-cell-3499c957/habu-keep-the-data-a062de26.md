---
title: Keep the DATA base from programs on every engine
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T21:52:26.059700+01:00"
---

Problem: the unlanded data-base rows (src/habu/prims.f:734-789 in jerry-dbp, srornlkv) make each row's package in every engine, sealed and empty where the engine does not load its file before the seal. src/lib/reason.f:22 is rejected E-SEALED-PACKAGE in Wasm builds (wasm-device, Maki's viewer), and Tender's package NATIVE (input-view.f:7) natively. Five public words still answer the DATA base: arena-pages.f DATA-OFF, TASK:MAIN-BASE, TASK:SELF-CONTEXT, DATA-VA, string.f SB-BUF. Acceptance: no program on any target names data-base or gets the DATA base from a public word; every Habu file a program loads today loads and runs on each target that loads it (wasm-device, maki-viewer, tender-server pass). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-dbp (design: /tmp/jerry-dbw-design-brief.md).
