---
title: Drive block-argument rows through structure gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:07:39.559082+02:00"
---

Full context: from agent storerows 2026-07-30 (dot habu-drive-unbound-storage-30b07943). No row in the shared structure vector tables adds a block argument, so IR-FUN:ARGS-CK's whole loop is dead in the gate. Two arms: the this-block arm is UNREACHABLE by any builder row - ADD-BLOCK-ARG writes the same b BCNT that END-BLOCK compares against and nothing between them can move the block count, a second authority checked to agree with itself - that reasoning is recorded in formal/Common/Structure.v BINDING GAPS and should stay text-pinned, do not fake a row for it. The kind arm IS drivable and undriven: an operation result falling inside an argument window must give E-IR-FUN-ARG. Closing it needs an argument column on the block step in test/compiler/ir-structure-schema.f, runner support in ir-structure-cases.f, and a vrow list in the generated Rocq block runner (ir-structure-obligations.f). Falsify by mutation: break the kind arm in src/compiler/ir/fun.f, the new row must red, restore green. Also relevant to the freeze verifier lane (habu-verify-frozen-compiler-224d78ad), whose block-argument-definition checks want the same fixtures.
