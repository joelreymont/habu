---
title: Write the T0 arm64 emitter
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.600949+02:00"
blocks:
  - habu-give-t0-one-fe9f04ec
---

Problem: arm64 compiles with the machine-code JIT; one T0 needs an arm64 emitter. Acceptance: src/habu/t0-arm64.f over src/arch/arm64/asm.f per umbrella section 3 (data stack x19 up, str/ldr x30 frame, BL + callmap outside region, DATA MOVZ/MOVK chain, ADR or four-lane chain for code addresses, VREGS from the scratch set, loop frames on the DATA loop stack, never the four-lane carrier for a scalar literal); provides itself to T0-EMIT; harness rows; borrowed-dispatch run of gate-stdlib-cases subsets. Prove BODYBUF parity between LBCAP capture and definers.f (quotation/does> signatures, string opener offset). Files: src/habu/t0-arm64.f (new), test rows. Verify: harness rows + borrowed-dispatch subsets on arm64. Depends: L1 (parallel with L4). Ownership: t0-arm64.f and its tests. Claim: unassigned.
