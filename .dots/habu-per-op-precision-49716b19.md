---
title: "Per-op precision grammar tokens (MATMUL:FP16)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T18:09:30.792542+02:00\""
---

Follow-up to habu-per-op-precision-4b64eee3 (landed d09b3ae1). The attr plumbing exists: maki/prec-attr.f CPREC-TAG stamps per-node compute precision, device lowering reads per-node attrs, workload default CPREC-DEFAULT! covers the inherited source. Missing: the explicit per-op source - grammar override tokens at the MODEL: body layer (e.g. MATMUL:FP16, LINEAR:BF16) parsed in maki/cad.f, mapping to CPREC-TAG on the captured node. Attribute plumbing + policy only, NO device codegen change. GEMM-class ops only (matmul/linear/equation), enforced by existing CPREC-GEMM-CK. Tests: round-trip capture asserting MIR-ATTR bits[33:32] per tagged op while untagged siblings keep the workload default; reject non-GEMM tag with E-CPREC-OP (-5431).

Claim: agent=precgrammar workspace=.jj-ws/precgrammar machine=spark
