---
title: opcode tables copied five times with a copy-paste defect
status: closed
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.902441+02:00"
closed-at: "2026-08-24T19:00:43.325798+02:00"
close-reason: "landed and verified: 0ba4111e integrated at 4f228ccb; focused compiler suites, judge/fuzz, maki, PTX, and full native suite green"
---

Problem: 76 O-* constants + SLOT-OF/SLOT-OPCODE + a 76-line BIND-DIALECT are repeated in src/compiler/native/combine.f:38-115,177-336,986-1061; spill.f:44-120,186-345,919-993; emit.f:46-122,242-401,1537-1612; partially in regalloc.f and regalloc-verify.f; the 46-opcode HIR side in select.f:51-97,236-335,2967-3012 and loop.f:44-91,169-268,1466-1518; COPY-ATTRS three times (spill.f:558-587, combine.f:685-747, loop.f:1144-1170). combine.f:335 throws E-A64SPILL-OPCODE inside package A64COMB (another pass's code). A64IR:opcode and HIR:opcode are closed ENUMs, but their declaration order is not the stable slot order. Acceptance: one ORD/NTH pair and one shared bind per dialect, load-time private constants derived from ORD for hot comparisons, the wrong code fixed, 1,059 net lines removed; chain suites and judge deterministic output byte-identical. The proposed shared COPY-ATTRS acceptance was refuted: spill, combine, and loop copy different attribute schemas and semantics, and sharing them would require callback machinery, so no shared copier. Files: the listed passes and owning dialects. Verify: test/compiler suites, judge --check. Depends: none. Ownership: native chain. Claim: unassigned.
