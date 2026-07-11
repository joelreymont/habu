---
title: "Autograd: higher-order grad (differentiate the backward kernel)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.517920+02:00"
---

PyTorch supports grad-of-grad (create_graph). Maki's AD-REVERSE is source-to-source, so the BACKWARD kernel is itself an ordinary checked kernel - run AD-REVERSE on it to get the second-order pass. Demonstrate on one nonlinear op (e.g. d2(EXP.)/dx2 or MUL): AD-REVERSE(forward) -> backward; AD-REVERSE(backward) -> 2nd-order; gradcheck the Hessian-vector product vs finite differences. Files: lib/ptx/ad.f + ad-test.f. VERIFY: 2nd-order numeric match. This is a place maki's source-to-source approach is structurally CLEANER than a runtime tape. Dep: EPIC; needs habu-ad-thread-saved (saved values) + habu-ad-validate-multi.

WALL PROBED 2026-07-11 (Wave-2 sweep, fail-closed evidence): grad-of-grad via a
second BW-BUILD composes TODAY only for pure-forward-op backward regions
(mul/add/matmul/linear/reshape/transpose/concat) - probe: MUL's combined IR
(fwd MUL + 2 backward OP-MUL) re-scans with no adjoint gap and the 2nd build
succeeds. It is fail-closed BLOCKED at every nonlinearity: adjoint.f
ADJ-DEFAULTS leaves every OP-*-BWD kind (gelu/relu/silu/layernorm/rmsnorm/
softmax-row/rope/rowsum/fullsum-dot/pad-scatter/scatter-add) ADJ-NONE, so
backward.f BW-STEP throws E-BW-NOADJ on the 2nd build (probe: GELU combined IR
-> bad-node-op=gelu-bwd). The real work: adjoints for the *-BWD op kinds (or
source-to-source re-expansion of the backward before differentiating), then a
second-order gradcheck. A d2(MUL)-only HVP slice would compose through existing
machinery but its BW-BUILD loss-seeding semantics need design first (flagged in
the sweep review).
