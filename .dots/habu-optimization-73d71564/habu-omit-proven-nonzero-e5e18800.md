---
title: Omit proven nonzero division guards
status: open
priority: 2
issue-type: task
created-at: "2026-09-28T13:16:47.895826+02:00"
---

Current B5 SHA24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b at sourcee337d11f emits a CBNZ,error literal,stack store and throw call before SDIV in MEM-64K-COUNT-FOR despite defining SSA divisor constant65536. Baked guard VM0x1000cbff0..ffc is16bytes avoidable at this proven site, not net-image saving. Native select.f:1186 and emit.f:1364 always choose guarded division. Prove scalar constant divisor nonzero at selection and choose explicit machine form/contract for unguarded SDIV; machine schema/verifier/layout/emission must agree. Unknown and literalzero retain guarded refusal, including catch/resume, signed truncation and MIN/-1 wrapping. Do not substitute shifts or infer facts from registers/address ranges. Extend genuine native literalzero/nonzero E2E before code only where coverage absent; reuse native-div-refusal. Measure instructions/callsites and total image, native convergence/full gate. Audit ~/.cache/tmp/habu-generated-code-audit-20260928-01.md. Lead integration/closure, Sol implementation in serialized codegen lane.
