---
title: chain ABI is Darwin with x18 reserved on the Linux host
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.899371+02:00"
---

Problem: src/compiler/native/abi.f:27-33 fixes the target identity to AAPCS64-DARWIN by construction and src/compiler/a64-effect.f:295 reserves x18 unconditionally; the engine now runs on Linux aarch64 where x18 is a plain callee-saved register and the platform ABI differs in the variadic/stack rules. Acceptance: the target identity comes from HB-TARGET (HB-TARGET-LINUX? / -MACOS?) with the Linux AAPCS64 rules stated and tested; x18 reserved only on Darwin; the native-chain suites and the judge green on this host; docs/porting.md names the seam. Files: src/compiler/native/abi.f, src/compiler/a64-effect.f, docs/porting.md. Verify: test/compiler suites under bin/hb on Linux. Depends: the recovery chain (needs an engine here). Ownership: native chain ABI. Claim: unassigned.
