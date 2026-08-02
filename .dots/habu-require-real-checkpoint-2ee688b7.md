---
title: Require real checkpoint fail-closed
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:29:05.186161+02:00"
closed-at: "2026-08-02T16:43:02.295241+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
blocks:
  - habu-migrate-alloc-and-0e587d85
  - habu-create-gpt2-fixture-63b55c1c
---

Why: codex blind review - the landed acceptance evidence for the real-model regression rode a presence-gated leg that silently SKIPs without the checkpoint while the suite reports test: ok; acceptance evidence must not be skippable. Also implements the accepted final-verdict item: the pinned checkpoint becomes mandatory in the DGX Spark release gate. Behavior: a dedicated fail-closed real-artifact entry - hard failure with a distinct code when the pinned checkpoint (sha 248dfc3911869ec493c76e65bf2fcf7f615828b0254c12b473182f0f81d3a707, 548105171 bytes) is absent or wrong - asserting absolute zero ownership counters at isolated entry and exit around the full real-path leg; the frozen entry is maki/infer/gpt2-payload-real.f: loading it fails distinctly when GPT2-FIXTURE reports the artifact absent or wrong, asserts absolute zero owner counters at isolated entry and exit, and executes the existing public GPT2PAY real leg; the acceptance command is exactly HB_TMP=<private> bin/hb --load maki/infer/gpt2-payload-real.f </dev/null (no vague aggregate-gate reference until an actual real-model gate exists); presence-gated legs remain for artifact-less dev boxes but are never acceptance evidence and their SKIP prints loudly. Owner: the gpt2 suite family plus the gate registration. Dependencies: habu-extract-real-checkpoint-8edea0cd (the provider owns the pinned constants). Acceptance: entry red without the artifact, green with it, counters zero at both stations; gate wiring proven by running the entry through the real gate path; diff lints clean. Real pre-change defect: measured - the suite printed real-model leg SKIPPED and exited 0 on a box without the artifact.
