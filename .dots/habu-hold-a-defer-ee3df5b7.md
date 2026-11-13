---
title: Hold a defer in a Wasm module
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:05:30.487732+01:00"
---

Problem: a Wasm program that declares a DEFER-LAYOUT-BUFFER and calls its -GROW does not link: with a program of ('SUMTYPE dlv VARIANT value n ;VARIANT ;SUMTYPE', 'DEFER-LAYOUT-BUFFER DL dlv', MAIN calling '2 DL-BIND 4 DL-GROW'), 'hb --load tools/hb-build.f -- --target wasm' on it on the installed hb stops with 'link: the defer STORAGE-CLEAR-XT needs a trailer, which a Wasm module cannot hold', 'link: a Wasm module cannot hold a defer' (src/habu/link-band.f:426-437 EM-TRAILER). -GROW reaches LDEFER-RETIRE (src/core/layout-buffer.f:266), which calls the defer STORAGE-CLEAR-XT (:199), so any program reaching a defer cannot target Wasm, while the same program builds and runs on arm64. Acceptance: that program builds for Wasm and its module prints what the arm64 build prints; a program whose code reaches any defer links for Wasm. Depends: none. Claim: unassigned.
