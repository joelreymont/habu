---
title: Reject duplicate ARM64 labels
status: active
priority: 1
issue-type: task
created-at: "2026-07-14T04:34:16.786541+02:00"
---

Full context: src/arch/arm64/icode.f LBL, overwrites an already defined LBLP entry, so branches compiled before and after a second binding silently target different locations. Root fix: make label definition one-shot and fail before mutation with exact 'icode: label redefined' diagnostic. Acceptance: first binding at code word zero is valid; a second binding at the same or different position exits 72; no label position, fixup chain, free list, pending count, or code word changes on rejection; forward/backward branch positives remain exact. Files: src/arch/arm64/icode.f, focused assembler regression, FILEMAP/LESSONS if needed. The fixup-chain reclamation prerequisite landed and closed at 4f9bd186. Claim: agent=arm64-duplicate-label workspace=.jj-ws/habu-reject-duplicate-arm64-8f9565fe.
