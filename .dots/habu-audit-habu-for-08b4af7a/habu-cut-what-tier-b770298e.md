---
title: Cut what tier 1 and the observers leave unread
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.450469+02:00"
---

Problem: after the observer deletion these have no reader: the native-unit-publish primitive (prims.f:644, habu1.f:4975, kernel-x64.f:3534), NATIVE-HOST-CELLS:SELECT, NHOST:RUN-REC (a bare XREF-START), and host.f's SOURCE-REFUSE, RANK, the reason constants, SOURCE-FREEZE/ARITY+/BEGIN/ABANDON and the FACT-* and SOURCE-* cells, whose writers sit in tier-1 files and t0.f:2040-2190. src/arch/arm64/icode.f:21-29 sizes AOT-SECTION-CAP by aot-file.f framing arm64 no longer loads. Acceptance: each is deleted or, where it still sizes something, states its arm64 reason. Depends: habu-delete-tier-1-dd3d6b86. Claim: unassigned.
