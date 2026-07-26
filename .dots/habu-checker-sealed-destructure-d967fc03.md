---
title: "Checker: sealed destructure and linear UNMAKE"
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:00:47.469920+02:00"
---

Problem: a sealed proof token can be forged by destructuring. maki/infer/model-config.f header lines 27-28 record the exposure: a REAL validated-config value can UNMAKE and re-MAKE with the stale proof and key, so holding the proof token does not prove the value passed validation; the accessors at lines 220-234 dup the value, UNMAKE the copy, and bind the proof, which is exactly the pattern that would let a caller reuse a stale proof. Required capability: sealed destructure - a structure family can declare that UNMAKE of a sealed value is package-private or consumes the value linearly (linear UNMAKE), so a proof-carrying value cannot be taken apart and reassembled outside its owning package. Acceptance: a minimal checked fixture that UNMAKEs a sealed value outside the owning package is rejected with a negative regression; model-config retires its header caveat and its proof token becomes forge-proof; existing in-package accessors still certify. Files: src/core/checker.f, the structure declaration front end, maki/infer/model-config.f when the caveat retires. Verify: checker and structure suites plus the model-config suite. Depends: none. Ownership: sealed destructure capability only. Claim: unassigned.
