---
title: Unify embedding on typed integer tokens
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T19:27:40.373420+02:00"
---

Why: forward decode produces integer token ids, but embedding lookup today reads ids out of a float tensor and provably accepts an out-of-range id. Two prior rulings on this surface (habu-add-int-token-c22473b8, twice today) forbid a second embedding row authority beside TOKPOS-EMBED, and the delivered EMBED-ROWS commit e59f701e is therefore preserved as rejected evidence only. Exact result: migrate the existing embedding path and every caller atomically to ONE typed integer-token representation - one admission pass bounding every id before any read (E-TOKEN-RANGE named code as landed in the evidence commit), one row-copy implementation, checked composed offsets, position and vocabulary extents both enforced, destination untouched on refusal; the float-id representation and its unbounded EMB-GATHER path are deleted in the same commit, not kept beside the new one. Owner: package MAKI in maki/embedding.f. Dependencies: joint refreeze review before dispatch; overlaps the typed-mutable-span design (habu-design-typed-mutable-76654024) - the admission/extent design must not conflict with it. Acceptance: embedding, autograd, pos-embed, gradcheck, and full maki suites green with byte-identical valid-id results; the id-99-against-3-rows probe rejects on every public path; swapped-argument and boundary mutations fail; structural probes prove exactly one row-copy implementation exists; both diff lints. Forbidden: a second lookup API, dual representations, clamping, behavior change for valid ids.

