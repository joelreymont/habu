---
title: Give a Wasm module run-time data space
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:20:59.742482+01:00"
---

Problem: 'here', 'allot' and 'align' reached from MAIN build and run on arm64, but a Wasm build refuses them ('link: a Wasm module has no here', test/wasm/hb-build.f:384-390 asserts it), so a program that sizes storage while running cannot target Wasm: 'SUMTYPE dlv VARIANT value n ;VARIANT ;SUMTYPE  DEFER-LAYOUT-BUFFER DL dlv  : MAIN ( -- ) 2 DL-BIND 4 DL-GROW 1 . ;' prints 1 on arm64, and DL-BIND, DL-GROW (src/core/layout-buffer.f LDEFER-BIND :273-275, LDEFER-GROW :298-300) and ADDRESS-CELLS:REMOVE-SPAN (src/habu/address-cells.f:180-181) reach 'here' and 'allot'. A module's image sits at WPROF:DATA-BASE and map-anon's pages begin past memory's last page (src/arch/wasm/profile.f:26-27, 40-43). Acceptance: a program that allots data space while running behaves the same on arm64 and Wasm; the DL program above builds for Wasm and prints 1. Depends: none. Claim: unassigned.
