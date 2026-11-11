---
title: Give a quotation type its block part
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T08:16:31.807466+02:00"
---

Problem: a quotation that reads locals must not outlive them (DECISIONS.md 'quotations read locals'); the quotation type (src/core/checker.f MK-QUOT :921) has four rows and side flags and nothing that says which block a quotation depends on, and the saved effect nodes (VR-QUOT :6555, EN-QUOT :10287) are full. Design (lead, from the checker census): every quotation term carries a block part, a type variable minted with the term; unbound = not decided, NSCOPE-flagged = must read no local (stored or kept), bound to a T-SCOPE = reads that block's locals; QUOT-PAIR unifies block parts; NO-SCOPE?* flags it; SCOPE-OCC?* searches it; copies (REQUOT, QUOT-INST, CALL-QUOT-COPY, :19519) keep it; VREC and EN nodes gain one field, no version number changes. Acceptance: no behaviour change: an engine built from the lane passes test/engine-suite.f, rigid-region-suite.f, c2-memory-refusals.f, c2-memory-scope-refusals.f, finally.f, empty-quotation.f. Files: src/core/checker.f. Verify: the brief's build and test loop. Depends: none. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/quot-a1-block-part.md. Ownership: src/core/checker.f in the lane. Claim: agent=jerry workspace=.jj-ws.noindex/quot-checker.
