---
title: Fix spill shape for a quotation carried across an IF
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-11T16:16:48.909168+03:00\\\""
closed-at: "2026-09-16T14:34:48.008620+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: A quotation retained across an IF still faults the spill shape on a valid program; residue is E-A64SPILL-SHAPE -8442 for that lowering."
---

Owner: /root/compiler_xhigh_review, workspace cedar-spill-trap, new change on80716458 (parent KEEP931a543c). The prior final-frame-token region bug is separately repaired/reviewed80716458; this is a new observed blocker.

On matched binary SHA d8c3aa00b54b28c16917a1c9e73bb5d686d202bc7585219a056f1d014f91b3fb, native-rstack advances past MODE-OF but fails NRS-QUOTE-BEFORE-IF with E-A64SPILL-SHAPE -8442 in5.478s. Standalone /tmp/cedar-keep-quote-join-spill.f reproduces in3.205s. It retains a quotation before an IF computes the other argument, then calls TTHROWSQ, matching Tender DOCX ROW-ENDINGS shape. KEEP actual-source assertions and native-quot/native-quot-scope/generic-calls/internal-call/many-locals pass on same binary. No full-suite acceptance.

Acceptance: fix the responsible spill shape/lowering fault, keep valid quotation-before-branch behavior with both branches and caught throw, preserve existing malformed spill/row rejections, pass native-rstack and focused neighbors. Do not rewrite Tender source or weaken spill validation. Root/Rowan review before integration; then real optimizing selfbuild.

Diagnosis: spill.f ONCE-CK at896 rejects before any block copy because it demands1 reserve/1 release and paired link save/load for every sibling function. Embedded never-returning quotation RAISE legitimately has reserve/link-save and no epilogue; selector/validator already support this. Owner is deriving each function epilogue requirements from actual return reachability, retaining entry reserve and returning-path link pairing.


Update 2026-09-11 13:59 UTC: Frozen cfcf1baf was independently cleared and is now included in integrated 92ef13f0. Matched binary 5f28af107fbcff650f3cf7335b00a07e672cccbe7553ac6e6fbbb4cb7fe6b464 passed native-rstack (6.273 s), native-regalloc, order-exit, loop-frame-order and quotation neighbors. Separate quotation-spill support is habu-support-spills-inside-11052a3b; do not conflate the two frame defects.
