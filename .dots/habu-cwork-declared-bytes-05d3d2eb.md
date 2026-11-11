---
title: "CWORK:DECLARED-BYTES and LAYOUT-MARK have no caller"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T07:36:24.215455+02:00"
---

Problem: package CWORK (src/compiler/workspace.f) keeps two public words nothing calls: DECLARED-BYTES and LAYOUT-MARK. The old builder found LAYOUT-MARK by name (s" CWORK:LAYOUT-MARK") and was deleted in 0306b422; rg -i --no-ignore over Habu (src tools test docs data spec), Zen, Tender, Maki and Etch finds no other use. Found by the review of the CWORK file-capture removal. Acceptance: both words and LAYOUT-MARK's comment are gone; the engine reaches a fixpoint and the suite passes. Removing a public word nothing calls is settled by the long-term rule. Files: src/compiler/workspace.f. Verify: chain check, suite and measure once at landing. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-cwork
