---
title: Reuse a register that holds a local in T0
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:28:55.569432+01:00"
---

Problem: T0 stores a bound local to its frame slot and loads it back though a register still holds it (src/habu/t0.f, EMIT-SLOT-STORE then EMIT-SLOT-LOAD): 16,028 of tenderd's 127,000 local loads on x86, 72 KB, and as many on arm64, with a store-to-load latency each. Acceptance: a fetch whose slot a register still holds emits nothing, on every target; code shrinks on arm64, x86-64 and Wasm; code-bench is no slower. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-t0-loc.
