---
title: "Infer pack: pack command"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:40:20.827328+02:00"
blocks:
  - habu-infer-pack-boring-c8e07d29
  - habu-infer-pack-bounded-7106d353
---

Why this exists:
the product contract requires one pack command joining normalized configuration, source tensors, layouts, bounded writing, and final verification.

Required result:
compose the landed pack modules into a checked CLI with explicit target, profile, and quantization selection; do not embed transform logic in the CLI.

Done when:
synthetic GPT-2 pack builds and reloads byte-identically; invalid flags and incompatible inputs reject before output publication; interrupted output is absent.

Expected touch points: the pack command, focused tests, and user documentation.
Smallest check: the focused pack-command process test.
Prerequisites: boring runtime loader and bounded tensor writer.
Owned result: CLI composition only.
Claim: unassigned.
