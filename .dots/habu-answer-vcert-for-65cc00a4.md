---
title: Answer VCert for construct-then-match
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:01:50.391473+02:00"
---

test/compiler/checker-model-manifest.f assert 703 red at tip: the shipped checker answers non-VCert for construct_then_match_returns_the_payload_it_was_given. CG-02 territory — the model says the obligation holds, the checker disagrees, and the manifest catches the divergence (working as designed). Decide which side is wrong by re-deriving the obligation, fix the checker (or the model statement, with the mutation proof), and keep the manifest assertion. Pre-existing at e04bd6fa, not introduced by the codegen merge.
