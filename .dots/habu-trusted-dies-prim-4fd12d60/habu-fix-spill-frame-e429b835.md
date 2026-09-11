---
title: Fix spill frame-token order on branches ending in traps
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.849496+03:00\""
---

Owner: /root/compiler_xhigh_review; coordinate with check_api, which owns elaborate.f/native-rstack.f. Separate workspace based on the frozen KEEP prototype. Own spill.f/regalloc-verify.f or the proven responsible scheduler layer; do not weaken validator checks.

New KEEP SSA-preservation prototype exposes E-A64RAV-ORDER -8522 compiling lib/test/suite.f MODE-OF. Minimal /tmp/cedar-keep-order-reduce.f fails in0.30s: a local survives C, nested IF branches call C, final ELSE throws. Suspected cause, not yet proved: spill.f creates frame-token lanes and the last frame-order token finishes in a predecessor of a trap; verifier currently permits unused final frame tokens only directly in trap blocks when function also returns.

Acceptance: reduced program compiles and runs on all returning/throwing branches, existing orphan/stale/multiple-consumer token rejections remain, focused spill/allocator/branch tests pass. Fix responsible ordering semantics; no A64RAV bypass. Review actual candidate before integration.

Diagnosis confirmed: orphan token39 comes from spill LOAD op32 in block4, which reaches only block6 ending in TRAP with no frame access. Return block8 is unreachable from block4. Spill threading is correct; TRAP-FRAME-END? rejects because a return exists elsewhere in the function. Verifier owner is replacing that global condition with actual reachable-region semantics, retaining LAST-FRAME-TOKEN and consumer/order checks. Tests owned: native-order-exit.f and native-regalloc.f, including real branch/caught-trap execution and negative reachable-return/frame-access controls. Diagnostic /tmp/cedar-spill-trap-diagnostic.log.

Frozen correction8071645840a1f66d83303d8e9339c576c67dd7ce, parent931a543c. Root actual-diff review clear. Exact reducer0.365s; native-order-exit0.515s; full native-regalloc16.942s including reachable-return/later-frame negative controls; dead-path4.023s,loop-frame-order5.725s,again10.886s all pass. Not integrated pending KEEP review. New -8442 quote-before-IF failure is habu-fix-spill-shape-5bb56182, not a recurrence of this order bug.
