---
title: "Waiting case: a Wasm module runs an execute whose word takes more cells than the stack holds"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T20:17:00.851775+02:00"
---

Problem: `: SHORT ( -- ) ['] DROP3 >NONE execute ;` (the xt of a 3-cell word, forged through a cast, run on an empty stack) runs to status 0 as a tier 0 module: every function is (ctx) -> (status), so call_indirect's type cannot refuse it and the callee reads below the data stack's base. Tier 1's module trapped on the signature (test/wasm/dynamic.f, deleted). The entry was dropped from test/wasm/calls.f; only a cast forges such a call, the checker refuses the plain form. Acceptance: once a goal program reaches it, the module traps there or the case is stated as unchecked code's own beside WCALLS. Files: test/wasm/calls.f, test/wasm/calls-test.f, src/arch/wasm/link-routines.f. Verify: SHORT built as a module's MAIN. Depends: none. Ownership: none. Claim: unassigned.
