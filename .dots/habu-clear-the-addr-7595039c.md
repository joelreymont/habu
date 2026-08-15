---
title: Clear the address map when pass two rewinds
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T14:40:32.880696+02:00"
---

PRIORITY 1 - BLOCKS THE CHAIN BAKE (e98b03d4 items 2-7), found by the bake-chain-3 checkpoint, PRE-EXISTING on master (proven by control swap): EM-P2-START (habu2.f:6894-era) rewinds CP to the colon entry and restores the DP watermark for the width-aware recompile, but pass 1's SNAP-RELOC:MARK-SITE bits over [entry, pass1-CP) are NEVER cleared - pass 2's stream has different lengths, so stale bits land on words holding no chain. 26 such bits in the chain window (25 from elaborate.f's wide-value words, 1 abi.f; owners STAGE-BINDING/COMMIT/STAGE-RECORD/LIT@/SPELL$/DO-EXIT - all pass-2 triggers); the engine's own EMIT-ADDRS and ACAP-UNCLASSIFIED both assert the invariant the stale bits break. Nobody hit it before: no prior window carried a pass-2 definition. FIX at the writer: clear the map bits over [entry, pass1-CP) in the same breath as the watermark restore (a bit-clear loop in the meta-assembler). Regression: a definition that triggers pass 2 AND emits a marked chain ([:  ;] / [']), stale bit asserted absent; the capture diagnostic classifier (scratchpad bc3/diag.f) is the instrument. REJECTED as patches: skipping non-chain-shaped sites at capture (silent tolerance, same bit reaches EMIT-ADDRS at restore); clearing the whole window map (discards live sites). Files: src/habu/habu2.f. Depends: none; e98b03d4 blocks on it.
