---
title: Agree on a fault in Habu code between native and Wasm
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T22:57:09.930839+02:00"
---

Problem: test/wasm/differential.f W07-TRAP (:266-269) expects the native run of a byte read at $FFFF under catch to die of SIGSEGV (CRASH-RC 139, :24) as the Wasm module traps. Since a174d43d src/habu/crash.f throws ENGINE-ERROR:MEMORY-FAULT (110) for a fault in Habu code, so the native run prints 2 then 110 and exits 0 with an E-MEMORY-FAULT line on stderr, and `bin/hb --load test/wasm/device.f` fails W07-TRAP (asserts 9 and 10). The device tests are outside the ordinary suite, so that change landed without them. Acceptance: native and Wasm give the same result for a fault in Habu code under catch, W07-TRAP asserts it, and test/wasm/device.f passes. Depends: none. Claim: unassigned.
