---
title: "docs per-file verdicts: archive, fold, delete"
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:26.047833+02:00"
---

Problem: per the review's verdict table - delete: docs/gpt2-activation-workspace.md (authority file gone), size-campaign.md (Linux goal met, numbers stale), habu-pitch.md, worker-briefing.md; archive (mechanism landed, retired, or never built): codegen-placement.md, triton-automation.md, critical-path.md, inference-engine-plan.md, batch-sequence-design.md, strided-views.md, golden-syntax.md, tma-gather.md (+case-tma-stride.md folded), extent-substrate.md, value-nominal-substrate.md, design-tfam-2b-i.md, design-tfam-5-redrive.md, registry-band.md, selfhost-subset.md, size-rca.md, ablation.md (with the paper); fold: argv.md/json.md/process-pty.md into stdlib.md, swiftforth-task-api.md into threads.md, compute-campaign.md into eval-triton.md, inference.md into ptx-sketch.md, short-stack.md into positioning.md, docs/maki/*.md into maki/README.md; fix: stdlib.md:9-10 'Planned', type-system.md:808-810 dtype claim, debugging.md:217-218 and registry-band.md:568-569 stale line refs, eval-triton.md/ptx.md marked Orin-era, gate.md:222-284 status log dropped. Acceptance: the moves done; every remaining doc referenced from README or docs/forth.md. Files: docs/. Verify: rg for each deleted name finds nothing. Depends: none. Ownership: docs. Claim: unassigned.
