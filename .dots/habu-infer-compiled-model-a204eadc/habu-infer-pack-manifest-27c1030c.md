---
title: "Infer pack: manifest schema"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:40:20.793583+02:00\""
blocks:
  - habu-add-generic-bounded-359c0944
  - habu-add-shared-inference-0dad1107
  - habu-integrate-reentrant-json-34850f2f
---

Why this exists:
the compiled model pack has no versioned typed manifest tying model identity, target, members, layouts, schedules, quality, and benchmark records together.

Required result:
define the canonical manifest schema and validation before writing pack bytes.

Done when:
canonical round-trip passes; duplicate member, unknown version, wrong target, missing required member, and conflicting kernel key reject before publication.

Expected touch points: new maki/infer/model-pack-manifest.f, new maki/infer/model-pack-manifest-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-pack-manifest-test.f.
Prerequisites: landed safetensors loader; habu-add-generic-bounded-359c0944; habu-add-shared-inference-0dad1107; habu-integrate-reentrant-json-34850f2f.
Owned result: manifest schema and validation only.
Claim: released.

Stale claim reconciled (2026-07-25): the peer orchestrator confirmed this lane dead in blackboard message 20260724-190033.997-codex-30ac on channel general, which states "I confirm the four old claims are stale: no live worker owns safetensors d3d3a8a6, normalized config 84fc05fa, manifest 27c1030c, or GPT-2 binding f2ed655d", and undertook to release them in the next metadata wave. The former packmanifest workspace .jj-ws/habu-infer-pack-manifest-27c1030c is evidence only. This contract is being superseded by the rev-4 inference leaf redesign posted as 20260724-191041.846-claude-7d24 on channel general, whose correction 1 keeps manifest and provenance work off the forward-execution path; do not implement from the description above until that redesign has replaced or re-frozen it.
