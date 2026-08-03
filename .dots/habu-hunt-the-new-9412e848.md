---
title: "Hunt the new codegen's losses adversarially"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-03T11:36:13.951245+02:00\""
---

Orchestrator challenge: the benchmarks flatter the new chain; build corpus4 explicitly designed to FIND rows where it is worse or slower, measured with the full methodology. Suspected loss classes, each a row: (1) CALL-FAN - a word calling five small (<40-byte) words in sequence: the old emitter INLINES such bodies verbatim at the call site (established in the call leaf: direct BL or verbatim inline copy for bodies <= 40 bytes) while the new chain emits a BL plus full data-stack save/restore per call; (2) CALL-LOOP-3 - three chain-callee calls per loop iteration with live values (T-SGD! amplified); (3) WIDE-ARITY - ( a b c d e f -- n ) arithmetic: entry pays one dload per argument; (4) LADDER - the INT-WIDENS? shape, eight sequential guarded early exits; (5) PRESSURE-LOOP - a loop body holding enough live values to force the new chain to spill (or hit its loop-carried-spill refusal - a refusal on a compilable-by-old word is reported as a gap-loss, which is also a finding); (6) BIG-CONSTS - several distinct 64-bit literals per iteration; (7) MANY-LOCALS - eight locals live across a loop; (8) TINY-CALLEE - a 2-instruction callee in a tight loop (pure call-overhead ratio); (9) FLOAT-MIX - int/float crossings inside a loop (fmov traffic); (10) STORE-LOAD-ALTERNATE - dependent ! then @ on one address per iteration (memory-order serialization). Every row: real compilable body, pinned inputs, old column committed, new column measured, results identical required, and the report lists EVERY loss with a per-row diagnosis (disassembly-level: which instructions the new code wastes) - the losses ARE the deliverable, not a failure. Existing three corpora byte-identical. Follow the corpus2/3 file conventions exactly.

Claim: agent=huntlane workspace=.jj-ws/habu-hunt-the-new-9412e848
