---
title: Attention scaffold erases operand roles (Q/K/V/O)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T19:06:32.278261+02:00\""
---

Found by the evaldev lane 2026-07-14 while building device numeric goldens: the attention authoring scaffold binds kernel operands POSITIONALLY (SETUP-EMIT hard-codes %rd1=Q..%rd4=O) and ATTN:STATE DISCARDS the four same-typed matrix tokens (2drop 2drop), so a candidate that swaps Q/K or routes output into V before ATTN:START emits BYTE-IDENTICAL PTX to the correct kernel - the 'wrong-but-green' attention pins were codegen no-ops, not miscompiled kernels, and no numeric golden can (or should) catch them. This is an operand-role ERASURE/expressiveness gap in the authoring scaffold, distinct from sumnorm's collective scaffold which threads register identity through ROW-SPAN and therefore manifests wrong code. Fix: thread operand roles through the attention scaffold (like ROW-SPAN identity threading) so role misuse either produces genuinely different code (then the device golden at maki/eval-emit-device.f catches it numerically) or is rejected by the checker as a role mismatch; then flip the two attention pins in maki/eval-emit-test.f from '[device: identical-PTX, codegen no-op]' to their real caught class. Files: the ATTN scaffold (lib/ptx/cg-attention.f / tools/ptx/attention-cg.f authoring words, ATTN:STATE/SETUP-EMIT), maki/eval-emit-test.f pins, maki/eval-emit-device.f fixtures. Verify: eval-emit + eval-emit-device suites, on-device golden run. Ownership: attention authoring scaffold.

Claim: agent=attnroles workspace=.jj-ws/fable-attnroles

ROLE THREADING LANDED 2026-07-14 (attnroles worker, "ptx: attention scaffold
binds operand roles", merged c7233098): ATTN:STATE packs the four operand
register numbers into attnctx (ATTN-PACK, one per byte lane) instead of
discarding the tokens; every phase loads/stores through its routed role
(ATTN-QREG..ATTN-OREG + ATTN-ADD-BASE) instead of hard-coded %rd1..%rd4 -
the ROW-SPAN analogue, so role misuse now emits genuinely different code and
joins the sumnorm swaps as a numeric-caught class. Correct emission proven
byte-identical (2581 bytes, empty diff vs base; GOLDEN-BYTES pin unchanged).
Negative regression lib/ptx/attention-roles-test.f: Q/K-swap PTX != correct
PTX AND stages from %rd2 / scores from %rd1 (register-exact), symmetric
output-into-V case, correct-order control still %rd1..%rd4. eval-emit pins
flipped to [device: numeric-caught]; device verdicts flipped to CAUGHT?
(assert divergence). REMAINING before close: the on-device golden rerun
(EVND:ATTN-QK-CAUGHT?/ATTN-OV-CAUGHT? are SKIP off-device) - run on the Orin
once the devproof lane frees it; predicted divergence > TOL ~9.8e-4 for both.
