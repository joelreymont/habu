---
title: "Infer: canonical model intake"
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:33:51.509258+02:00"
blocks:
  - habu-parse-gpt-2-c8baa4db
  - habu-delete-unused-config-a6f7d6dc
  - habu-delete-unused-llama-ff039e5e
  - habu-own-gpt-2-14415dcd
  - habu-use-canonical-checkpoint-92eac785
  - habu-prove-sealed-inference-1d007ad5
---

Campaign only; do not dispatch. Hard-cut model metadata before device execution. MDLCFG is the sole normalized configuration authority, MAKI:dtype is the sole dtype authority, GPT2TENSOR and the later Qwen catalog own tensor roles, and SAFET is the sole checkpoint parser used by the direct device loaders. Delete schema versions, duplicate config identity, and duplicate numeric dtype codes; add no compatibility reader, host model, pack prerequisite, second normalized record, or raw tensor span. Close when pinned config.json produces one exact MDLCFG value and the direct GPT2DEV loader can validate every tensor through GPT2TENSOR with wrong or duplicate fields, shapes, dtypes, and roles rejected before publication. Supersedes the removed pack and host-forward contracts at source commit 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8. Claim: unassigned.
