---
title: "MODEL: DSL capability: A@B^T / input-transpose"
status: open
priority: 3
issue-type: task
created-at: "2026-07-11T19:57:43.537789+02:00"
---

From the GC-ATTN composite attention gradcheck (fable 44dde089): the single-running-value MODEL: DSL cannot express Q @ K^T - a TRANSPOSE of a non-running input operand (and there is no A@B^T fused op). GC-ATTN works around it by supplying K pre-transposed (kt:DxL). Candidate fixes: an input-transpose reference form in the MODEL: grammar (e.g. w^T naming) or an A@B^T matmul variant op with its own adjoint. Needed for natural attention definitions; not urgent (workaround is sound and gradchecked).
