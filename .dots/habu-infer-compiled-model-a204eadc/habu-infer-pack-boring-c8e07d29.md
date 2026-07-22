---
title: "Infer pack: boring runtime loader"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.821589+02:00"
blocks:
  - habu-infer-pack-bounded-7106d353
  - habu-infer-pack-manifest-27c1030c
---

Why this exists:
runtime loading must map and validate a completed pack without rediscovering model layout or registering partial state.

Required result:
validate the full manifest, member table, checksums, target, and compatibility first, then publish one immutable loaded-pack handle with owned mappings.

Done when:
each corrupt member class rejects before publication; successful load exposes only typed member spans; unload returns every mapping exactly once and stale handles reject.

Expected touch points: new maki/infer/model-pack-load.f, new maki/infer/model-pack-load-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-pack-load-test.f.
Prerequisites: bounded tensor writer and manifest schema.
Owned result: runtime pack validation, mapping, and lifetime only.
Claim: unassigned.
