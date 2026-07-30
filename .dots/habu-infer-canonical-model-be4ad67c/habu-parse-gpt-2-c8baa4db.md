---
title: Parse GPT-2 model config
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:25.639867+02:00"
blocks:
  - habu-delete-model-config-1c71a13e
  - habu-use-canonical-checkpoint-92eac785
  - habu-pin-gpt-2-cdb5cfe0
  - habu-own-model-asset-c6f938e4
---

Problem: production has no path from the pinned config file to MDLCFG:mcfg; tests construct config directly. Result: HFCFG:OPEN-GPT2 takes and returns MODEL-ASSET:ws with a root ptr u8 plus CAD-NUM:byte-len, preflights and joins the GPT2PIN config basename against FS-PATH-CAP in that workspace, checks exact length and SHA-256 over the same buffer it parses, consumes exactly the required openai-community/gpt2 fields through JR, and delegates semantic validation to MDLCFG:BUILD. Every result arm returns the workspace. Unsafe or overlong root, missing, duplicate, wrong-role, overflowing, unsupported architecture or dtype, and digest mismatch reject before publication. HFCFG defines no second config, tensor catalog, default, chat policy, or pack. Owner: new maki/infer/hf-config.f GPT-2 file parser and focused test. Production red: no non-test artifact path constructs MDLCFG from config.json. Acceptance: the pinned root opens and hashes the file once and yields the exact config; hostile structure and mutated files fail with the workspace intact; HFCFG, JSON reader, and model-config gates pass. Forbidden: package-global path or file buffer, verified-root value, caller-supplied identity, DOM, schema, compatibility field, fallback, or model-name heuristic. Smallest owning check: bin/hb --load maki/infer/hf-config-test.f. Claim: unassigned.
