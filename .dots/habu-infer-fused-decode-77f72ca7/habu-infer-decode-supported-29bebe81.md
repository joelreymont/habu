---
title: "Infer decode: supported geometry contract"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.327832+02:00"
blocks:
  - habu-infer-gpt2-checked-54b99423
---

Why this exists:
decode variants need one fail-closed contract for target, dtype, head dimensions, page tokens, MHA/GQA/MQA head mapping, context regime, and batch bound before launch.

Required result:
define typed geometry and derive every stride and launch extent with checked arithmetic.

Done when:
supported GPT-2 and pinned modern-model rows pass; invalid divisibility, dtype, page size, target, context, and overflow reject before emitter or launch.

Expected touch points: new lib/ptx/decode-geometry.f, focused test, FILEMAP.md.
Smallest check: bin/hb --load the focused geometry test.
Prerequisites: GPT-2 checked geometry for its concrete row.
Owned result: decode geometry and legality only.
Claim: unassigned.
