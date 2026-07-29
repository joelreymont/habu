---
title: "Infer: canonical model intake"
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:33:51.509258+02:00"
---

Campaign only; do not dispatch. Hard-cut the current model metadata and checkpoint intake before forward execution. MDLCFG is the sole normalized configuration authority, MAKI:dtype is the sole dtype authority, GPT2TENSOR and the later Qwen tensor catalog own role-to-slot mapping, SAFET is the sole checkpoint parser, and WSTORE is the sole weight owner. Delete schema versions and duplicate numeric dtype codes; add no compatibility reader, pack prerequisite, second normalized record, or raw tensor span. The campaign closes when a real GPT-2 config.json plus pinned safetensors artifact produces one validated GPT2LOAD model through this chain and every wrong or duplicate field, tensor, shape, dtype, or identity rejects before publication. Supersedes the removed pack-normalized and pack-tensor contracts at source commit 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8. Claim: unassigned.
