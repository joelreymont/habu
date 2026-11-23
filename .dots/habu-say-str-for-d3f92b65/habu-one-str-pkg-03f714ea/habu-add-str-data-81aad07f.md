---
title: "Add STR:DATA and STR:LEN primitives"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:37:56.048957+01:00"
---

Problem: a str's base and count are reached through str> and a stack shuffle (Joel 2026-11-23: STR:DATA and STR:LEN, inlined to tight code); design .out/designs/design-str.md decision 2, lane 3. Acceptance: STR:DATA ( str -- ^u8 ) and STR:LEN ( str -- n ) are STR-owned engine primitives with bodies in the arm64, x86-64 and Wasm kernels, published by src/lib/string.f. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-strprim.
