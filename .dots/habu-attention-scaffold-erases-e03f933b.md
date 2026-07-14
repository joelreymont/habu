---
title: Attention scaffold erases operand roles (Q/K/V/O)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T19:06:32.278261+02:00\""
---

Found by the evaldev lane 2026-07-14 while building device numeric goldens: the attention authoring scaffold binds kernel operands POSITIONALLY (SETUP-EMIT hard-codes %rd1=Q..%rd4=O) and ATTN:STATE DISCARDS the four same-typed matrix tokens (2drop 2drop), so a candidate that swaps Q/K or routes output into V before ATTN:START emits BYTE-IDENTICAL PTX to the correct kernel - the 'wrong-but-green' attention pins were codegen no-ops, not miscompiled kernels, and no numeric golden can (or should) catch them. This is an operand-role ERASURE/expressiveness gap in the authoring scaffold, distinct from sumnorm's collective scaffold which threads register identity through ROW-SPAN and therefore manifests wrong code. Fix: thread operand roles through the attention scaffold (like ROW-SPAN identity threading) so role misuse either produces genuinely different code (then the device golden at maki/eval-emit-device.f catches it numerically) or is rejected by the checker as a role mismatch; then flip the two attention pins in maki/eval-emit-test.f from '[device: identical-PTX, codegen no-op]' to their real caught class. Files: the ATTN scaffold (lib/ptx/cg-attention.f / tools/ptx/attention-cg.f authoring words, ATTN:STATE/SETUP-EMIT), maki/eval-emit-test.f pins, maki/eval-emit-device.f fixtures. Verify: eval-emit + eval-emit-device suites, on-device golden run. Ownership: attention authoring scaffold.

Claim: agent=attnroles workspace=.jj-ws/fable-attnroles
