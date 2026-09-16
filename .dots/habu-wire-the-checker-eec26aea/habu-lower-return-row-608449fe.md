---
title: Lower return-row effects at native call boundaries
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-13T21:51:55.550637+03:00\""
closed-at: "2026-09-16T14:34:47.968873+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Native callable rows still cannot carry callee return-vector motion; residue is a typed return-row ABI or an enforced and diagnosed cross-tier boundary."
---

Found while verifying restored payload consumers. An ordinary checked PROVIDER ( n | -- | n ) >r followed by tier-1 CONSUMER ( n -- n ) PROVIDER r> refuses -8286 at PROVIDER before any payload capture. Exact source-owner fixture/log: build/payload/native-return-call-control.f and .log in .jj-ws/cedar-owner-payload. Source confirms src/compiler/native/hir-word.f RESOLVE-CALLABLE rejects neutral=false because callable rows cannot carry callee return-vector motion. The JIT accepts the original and restored metadata; this is an existing native call contract limit, not evidence that return-row graph preservation is complete under AOT. Extend callable/elaboration ABI with typed return-vector inputs/results consistent with the SSA return stack, or define and enforce a supported cross-tier boundary. Test data/return shared variables, heterogeneous return values, nested calls, exceptional exits, live caller return slots, and imported metadata with actual JIT/native results. Preserve caller vector ownership and forbid stale memory-stack assumptions. Keep graph/native acceptance open until the relevant consumer works.
