---
title: One public word turns a bound arm64 label into a byte offset
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:39:00.459694+02:00"
---

Problem: three copies of 'label to byte offset': src/arch/arm64/icode.f :654 DLBL, src/arch/arm64/link-kernel.f :437 OFFSET, and src/arch/arm64/kernel-object.f :195 CODE-OFFSET (added by priv-step1; review finding F5, .jj-ws.noindex/jerry-wasm/.out/review-priv-step1-findings.md). Acceptance: one public word in icode.f beside the label store; the other two call it and their copies are deleted. Files: the three named. Verify: an engine built twice from the lane, the second equal to the first up to the signature. Depends: priv-step1 lands (habu-locate-the-kernel-5bb85020). Ownership: those lines. Claim: unassigned.
