---
title: Parse GPT-2 model config
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:25.639867+02:00"
blocks:
  - habu-delete-model-config-1c71a13e
---

Problem: production GPT2LOAD has no caller that reads a Hugging Face config.json into MDLCFG:mcfg; current tests construct configuration directly, so a real checkpoint cannot enter through one complete production path. Result: package HFCFG exports PARSE-GPT2 ( JR:reader -- result<MDLCFG:mcfg,n> ). It accepts exactly the pinned openai-community/gpt2 fields needed by MDLCFG, rejects missing, duplicate, wrong-role, overflowing, unknown-required, and unsupported architecture or dtype fields before MDLCFG publication, and delegates semantic validation to MDLCFG:BUILD. HFCFG owns JSON-to-semantic adaptation only; it defines no configuration record, tensor catalog, defaulted compatibility field, chat policy, or pack representation. Dependencies: habu-delete-model-config-1c71a13e and habu-use-canonical-checkpoint-92eac785. Owner: new maki/infer/hf-config.f and its focused production-path test. Production red: no non-test artifact path can construct MDLCFG from config.json. Acceptance: the pinned GPT-2 config and safetensors identity load through HFCFG then GPT2LOAD; hostile comments, strings, duplicate keys, reordering, wrong roles, overflow, and unsupported model_type cannot satisfy parsing; focused HFCFG, JSON reader, model-config, GPT2LOAD, and exact diff gates pass. Claim: unassigned.
