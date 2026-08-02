---
title: Delete unused Llama config arm
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T21:11:39.314630+02:00"
---

Problem: MDLCFG:arch includes llama fields and validation, but no model, parser, loader, or production entry constructs that arm. Result: delete the llama variant, GQA/RoPE/RMSNorm fields and accessors that exist only for it, E-GQA and any other now-unused diagnostics, its tests, and foreign-family fixtures that manufacture it solely to test GPT-2 rejection. Keep GPT-2 architecture and all real loader rejection coverage. Do not retain a reserved tag, compatibility arm, generic dense placeholder, version, or default. The pinned Qwen2.5 leaf later adds its exact qwen2 arm and fields when it has a real parser/loader caller. Owner: MDLCFG unused llama-only source/test sites. Production red: XREF finds no llama constructor call outside tests. Acceptance: no llama symbol/tag/field remains; old constructor does not resolve; GPT-2 config/hash/load suites, package gate, native fixpoint, and exact diff gates pass. Claim: unassigned.
