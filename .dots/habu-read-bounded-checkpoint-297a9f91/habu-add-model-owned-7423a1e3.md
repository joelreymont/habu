---
title: Read GPT-2 host tensor
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T17:13:41.385140+02:00"
blocks:
  - habu-add-bounded-u32-9bd95c8c
  - habu-carry-model-config-c9085fa1
  - habu-own-gpt-2-14415dcd
  - habu-use-canonical-checkpoint-92eac785
---

Why: nothing can read a weight element from a loaded model; this is the first read on the model-forward critical path. Exact result: public GPT2LOAD:TENSOR-F32@? ( gpt2-model GPT2TENSOR:tensor-id CAD-NUM:index -- gpt2-model option<r> ) lands inside the existing GPT2LOAD owner in maki/infer/gpt2-load.f. Owner-construction packages cannot be reopened, so no gpt2-read.f or forwarding package is allowed. While the model is intact, MODEL-CONFIG and GPT2TENSOR:SLOT validate the tensor and nominal layer index against the consuming config and return the typed slot. Derive the element-relative CAD-NUM:byte-off with CAD-NUM:INDEX-BYTE-OFF and the named four-byte width before UNMAKE. UNMAKE the model once inside GPT2LOAD, call WSTORE:U32-LE@?, rebuild once with the returned store, and map some(bits) through F32:WIDEN into option<r>. Out-of-extent and store refusal return none. PREPARE already proves every GPT-2 tensor is F32, so no per-read dtype dispatch exists. No raw owner cell, pointer, span, global, parked state, or new trust crosses the boundary. Owner: existing maki/infer/gpt2-load.f and a focused external consumer test that does not reopen GPT2LOAD. Production red: no word can read one value through an owned loaded model. Acceptance: a deterministic real GPT2LOAD fixture reads fixed independent bytes; mapped and copied models agree; layer zero/last, last-valid/first-invalid element, index overflow, and store refusal follow their exact outcomes; missing optional artifacts cannot satisfy acceptance; old copied-config-key rejection is absent after its hard cut; full GPT2LOAD/WSTORE/Maki and existing exact-diff gates pass. Forbidden: raw spans, private pointer readers, throws for data refusal, skipped-fixture acceptance, second geometry authority, reopening GPT2LOAD, or touching maki/infer/gpt2.f.
