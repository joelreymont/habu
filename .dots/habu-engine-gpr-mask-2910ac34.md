---
title: "ENGINE-GPR:MASK has no caller"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T17:30:08.298749+02:00"
---

Problem: src/habu/layout.f ENGINE-GPR:MASK (the target-selected register mask) is called nowhere: src/arch/arm64/machine.f and src/compiler/native/x64ir.f read A64-MASK and X64-MASK by name and say so in their comments (rg 'ENGINE-GPR:MASK' finds only those two comments). Acceptance: MASK is removed and the two comments no longer name it; an engine built before and after is byte-identical before the signature unless the word's own bytes go. Files: src/habu/layout.f, src/arch/arm64/machine.f, src/compiler/native/x64ir.f. Verify: rg finds no ENGINE-GPR:MASK; the engine builds. Depends: none. Ownership: none. Claim: unassigned.
