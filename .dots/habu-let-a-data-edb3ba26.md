---
title: Let a data-stack routine spill
status: open
priority: 3
issue-type: task
created-at: "2026-07-31T22:46:28.200755+02:00"
---

A routine whose convention names data-stack slots and whose values do not all fit in its registers needs both the entry sequence and a frame, and there is no rule for nesting them: A64RAV's FRAME-CK requires the frame reserve to be the block's first operation and DSTACK-CK requires the data-stack take to be, so src/compiler/native/regalloc-verify.f refuses the combination by name (E-A64RAV-DSTACK). No pass in the chain builds one today - test/compiler/native-chain-fixture.f's FINISH-HABU does not run the spill lowering - so nothing is broken, but a Habu-convention word with more live values than registers cannot be compiled. The fix is an order both checks agree on (take, entry loads, reserve, body, release, exit stores, publish) and a FRAME-CK that finds its reserve at the first non-data-stack position. Owners: A64SPILL, A64RAV.
