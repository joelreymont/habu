---
title: "Infer decode: supported geometry contract"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.327832+02:00"
blocks:
  - habu-prove-gb10-inference-d43eecce
  - habu-parse-gpt-2-c8baa4db
  - habu-use-canonical-checkpoint-92eac785
  - habu-add-kv-layer-41961bed
---

Why this exists:
the GPT-2 device executor needs one fail-closed contract for target, F32 dtype, head count, head dimension, page tokens, context length, and batch-one launch bounds before attention launches.

Required result:
package DECODEGEOM owns one typed geometry derived only from validated MDLCFG, the canonical checkpoint dtype, KV page geometry, and the probed GB10 target. Derive every stride, byte extent, mask bound, grid dimension, and launch extent with checked arithmetic. Do not add generic MHA/GQA/MQA selection; the later modern-model adapter extends this owner when its exact row is pinned.

Done when:
the real GPT-2 row passes; wrong family, non-F32 dtype, non-divisible heads, zero or oversized page/context, wrong target, and arithmetic overflow reject before emitter or launch.

Expected touch points: new lib/ptx/decode-geometry.f and focused test.
Smallest check: bin/hb --load the focused geometry test.
Prerequisites: canonical GPT-2 configuration, dtype, and GB10 target.
Owned result: decode geometry and legality only.
Claim: unassigned.
