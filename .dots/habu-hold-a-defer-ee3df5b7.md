---
title: Hold a defer in a Wasm module
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T22:05:30.487732+01:00"
---

Problem: a Wasm program whose code reaches a defer does not link: with 'defer HOOK ( n -- n ) : TWICE ( n -- n ) 2 * ; : SET ( -- ) [: TWICE ;] is HOOK ; SET : MAIN ( -- ) 3 HOOK . ;', 'hb --load tools/hb-build.f -- --target wasm' on the installed hb stops with 'link: the defer HOOK needs a trailer, which a Wasm module cannot hold' (src/habu/link-band.f:426-437 EM-TRAILER), while its arm64 build prints 6. A trailer is dictionary metadata and a stripped program has no dictionary, and a run-time 'is' calls xt!, which the module's routines lack (src/arch/wasm/link-routines.f ROUTINE-BODY). Acceptance: a program whose code reaches a defer, installed while building, stored by 'is' while running or unset, links for Wasm and its module prints what its arm64 build prints; a stripped program lays and reads no trailer on any target. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wdefer.
