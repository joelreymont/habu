---
title: "Infer pack: tensor layout catalog"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.810211+02:00"
blocks:
  - habu-infer-pack-normalized-84fc05fa
  - habu-infer-pack-manifest-27c1030c
---

Why this exists:
packed weights need explicit source orientation, final orientation, dtype, quantization block, alignment, and kernel compatibility rather than name conventions.

Required result:
define a typed tensor-layout catalog keyed by normalized tensor role and validate every required tensor exactly once.

Done when:
GPT-2 Conv1D orientation is explicit; missing role, duplicate role, incompatible dtype/layout, and unsupported kernel key reject before packing.

Expected touch points: new maki/infer/model-pack-layout.f, new maki/infer/model-pack-layout-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-pack-layout-test.f.
Prerequisites: normalized model config and manifest schema.
Owned result: tensor role and layout metadata only.
Claim: unassigned.
