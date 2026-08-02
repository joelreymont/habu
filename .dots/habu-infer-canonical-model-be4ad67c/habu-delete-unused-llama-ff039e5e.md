---
title: Delete unused Llama config arm
status: closed
priority: 1
issue-type: task
created-at: "2026-07-29T21:11:39.314630+02:00"
closed-at: "2026-08-03T00:43:36.475345+02:00"
close-reason: Unused Llama config surface deleted in 4b235664; full Maki and native stdlib/PTX passed on the exact tree.
---

Problem: MDLCFG:arch includes llama fields and validation, but no model, parser, loader, or production entry constructs that arm. Result: delete the llama variant, GQA/RoPE/RMSNorm fields and accessors that exist only for it, E-GQA and any other now-unused diagnostics, its tests, and foreign-family fixtures that manufacture it solely to test GPT-2 rejection. Keep GPT-2 architecture and all real loader rejection coverage. Do not retain a reserved tag, compatibility arm, generic dense placeholder, version, or default. Owner: MDLCFG unused llama-only source/test sites. Production red: XREF finds no llama constructor call outside tests. Acceptance: no llama symbol/tag/field remains; old constructor does not resolve; GPT-2 config/hash/load suites, package gate, native fixpoint, and exact diff gates pass. Claim: agent=codex workspace=.jj-ws/delete-llama-current.
