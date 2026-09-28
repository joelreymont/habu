---
title: Omit proven nonzero division guards
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-28T13:16:47.895826+02:00\\\"\""
closed-at: "2026-09-28T15:46:37.459708+02:00"
close-reason: "Select unguarded SDIV only for structurally proven nonzero scalar constants; unknown and zero retain guarded refusal. Exact source 78d21b47 passed independent review, B2–B5 and names equality, all 492 suites, strict signatures and both Maki board smokes. Engine AOT code falls 4968 bytes and 344 throw-call entries disappear; file remains 2790775 bytes due alignment. Maki code falls 18072 bytes and file falls 32832 to 20553248 bytes; both board hashes unchanged. Engine SHA256 ae41d43ac92b69d6c0a1bc45b92f4f359a77b7766fd991e2fa4930e10b83ca02. Receipt: ~/.cache/tmp/habu-native-divisor-completion-20260928-01.md."
---

Current B5 SHA24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b at sourcee337d11f emits a CBNZ,error literal,stack store and throw call before SDIV in MEM-64K-COUNT-FOR despite defining SSA divisor constant65536. Baked guard VM0x1000cbff0..ffc is16bytes avoidable at this proven site, not net-image saving. Native select.f:1186 and emit.f:1364 always choose guarded division. Prove scalar constant divisor nonzero at selection and choose explicit machine form/contract for unguarded SDIV; machine schema/verifier/layout/emission must agree. Unknown and literalzero retain guarded refusal, including catch/resume, signed truncation and MIN/-1 wrapping. Do not substitute shifts or infer facts from registers/address ranges. Extend genuine native literalzero/nonzero E2E before code only where coverage absent; reuse native-div-refusal. Measure instructions/callsites and total image, native convergence/full gate. Audit ~/.cache/tmp/habu-generated-code-audit-20260928-01.md. Lead integration/closure, Sol implementation in serialized codegen lane.
