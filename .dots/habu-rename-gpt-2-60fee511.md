---
title: Rename GPT-2 tensor API
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T19:23:40.703222+02:00"
---

Why: package GPT2BIND does not bind or load anything. It defines the canonical identities, names, shapes, orientations, and slots for tensors in a GPT-2 checkpoint. The current package and short type names hide that responsibility.

Owner and exact interface: rename maki/infer/gpt2-roles.f to maki/infer/gpt2-tensor.f and package GPT2BIND to GPT2TENSOR. Rename grole to global-role, brole to layer-role, orient to orientation, gb-proof to layer-proof, layerid to layer-id, and tid to tensor-id. In tensor-id, rename variant block to layer. Rename public words LAYER to LAYER-ID, CENSUS-COUNT to COUNT, COPY-KEY? to COPY-NAME?, TID-SHAPE to SHAPE, TID-ORIENT to ORIENTATION, and TID-SLOT to SLOT; keep FORMAT-ID. Rename E-GB-LAYER to E-LAYER, E-GB-FOREIGN to E-CONFIG, and E-GB-EXTENT to E-SIZE. Generated constructor, matcher, equality, MAKE, and UNMAKE names change with their package and declaration names.

Behavior and representation do not change: the same four global roles, thirteen layer roles, exact Hugging Face names, shapes, Conv1D orientations, configuration-authenticated layer IDs, slot order, overflow checks, and copy-out contract remain authoritative. All direct consumers change atomically to the new interface.

Owned write set: rename gpt2-roles.f and gpt2-roles-test.f; update gpt2-bind-fixture.f, gpt2-bind-test.f, gpt2-bind.f, model-provenance.f, model-types.f, maki/test.f, maki/test-core.f, tools/refine-lint-core.f, tools/enum-census-baseline.txt, FILEMAP.md, TRUSTED.md, and only current task or lesson text that names this exact tensor API. Do not alter checkpoint-load behavior.

Forbidden: aliases, forwarding words, duplicate declarations, compatibility files, semantic changes, new tensor kinds, new validation, broad replacement of unrelated uses of bind, or keeping an old generated name reachable.

Production defect and acceptance: before the change, the checked GPT-2 tensor suite and its consumers resolve GPT2BIND while GPT2TENSOR is absent. After the change, bin/hb --load maki/infer/gpt2-tensor-test.f and every direct consumer suite pass through the renamed production package; exact symbol probes accept each new type and reject every old package, type, word, and generated constructor; the enum census is regenerated from the real declarations; repository search finds no GPT2BIND or gpt2-roles reference outside immutable history. Exact package and typed-local diff gates pass. This is a pure hard cutover and one compiling commit.
