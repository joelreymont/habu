---
title: error codes minted outside errors.f and a drifting map
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.961934+02:00"
---

Problem: lib/errors.f claims ownership but codes are minted in content-key.f:70-71 (-6920/-6921, in a range errors.f:445 calls unclaimed), render.f:537 (-6210), type/deftype.f:112-113, ptx/cuda-driver.f:367 (-5002, inside the maki range), ptx/sentinel.f:695, ptx/cg-mma.f (-6100..-6111), ieee754.f:59, unicode/class-data.f, codegen.f:543-544, cad-num-types.f:67-72, cad-num-arithmetic.f:231; errors.f:429-434 labels COMPILER-STORE-PROOF -6820..-6839 but defines -6840..-6859; :445 says -6880..-6999 unclaimed while :474 defines -6880; maki/tensor.f:5-6 claims -5000..-5099 while maki uses to -5699 and -7600..-7699. errors.f is 1309 lines, a third narrative. Acceptance: error-code-lint extended to refuse a code constant defined outside errors.f (or a registered owner file), the map corrected, the narrative moved to docs/. Files: lib/errors.f, tools/error-code-lint-core.f, the minting files. Verify: tools/error-code-lint.f exit 0 with the new rule. Depends: none. Ownership: error codes. Claim: unassigned.
