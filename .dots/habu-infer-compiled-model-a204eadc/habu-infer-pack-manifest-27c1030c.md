---
title: "Infer pack: manifest schema"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.793583+02:00"
---

Why this exists:
the compiled model pack has no versioned typed manifest tying model identity, target, members, layouts, schedules, quality, and benchmark records together.

Required result:
define the canonical manifest schema and validation before writing pack bytes.

Done when:
canonical round-trip passes; duplicate member, unknown version, wrong target, missing required member, and conflicting kernel key reject before publication.

Expected touch points: new maki/infer/model-pack-manifest.f, new maki/infer/model-pack-manifest-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-pack-manifest-test.f.
Prerequisites: landed safetensors loader.
Owned result: manifest schema and validation only.
Claim: unassigned.
