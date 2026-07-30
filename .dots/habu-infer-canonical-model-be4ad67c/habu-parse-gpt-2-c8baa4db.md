---
title: Parse GPT-2 model config
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:25.639867+02:00"
blocks:
  - habu-pin-omitted-gpt-6b395d6c
---

Problem: production has no path from the pinned config file to MDLCFG:mcfg; tests construct config directly. Result: HFCFG:OPEN-GPT2 ( ptr u8 CAD-NUM:byte-len -- HFCFG:open-result ) returns exactly opened(MDLCFG:mcfg) or refused(code). It allocates scoped path, file, digest, and JR storage internally through MEM:WITH-BYTES; joins only GPT2PIN:CONFIG-NAME$ to the borrowed root; reads exactly GPT2PIN:CONFIG-LEN bytes; hashes and parses that same byte span once; compares GPT2PIN:CONFIG-SHA256$; consumes every explicit required openai-community/gpt2 field through JR; takes the omitted resolved revision facts only from GPT2PIN:DTYPE, GPT2PIN:TIED?, and GPT2PIN:ATTN-SCALE?; then delegates semantic validation to MDLCFG:BUILD. Empty roots, embedded NUL, joined paths over FS-PATH-CAP, missing or extra bytes, duplicate or missing keys, wrong JSON roles, overflow, unsupported architecture, semantic rejection, and digest mismatch return refused without publishing a config. Owner: new `maki/infer/hf-config.f` and its focused test only. Production red: no non-test artifact path constructs MDLCFG from pinned config bytes. Acceptance: the real pinned root opens one file and yields the exact current GPT-2 config; hostile and mutated files exercise the real open/read/hash/JR path; two sequential opens reuse no HFCFG storage; every refusal releases every scoped mapping; HFCFG, memory, JSON reader, model-config, typed-local, and package gates pass. Forbidden: MODEL-ASSET, caller-visible storage, package-global path/file/JR buffer, second config or dtype authority, Hugging Face default resolution, caller-supplied basename/length/digest, verified-root value, DOM, schema, version, compatibility field, fallback, model-name heuristic, or unrelated parser framework. Smallest owning check: `bin/hb --load maki/infer/hf-config-test.f`. Claim: unassigned.
