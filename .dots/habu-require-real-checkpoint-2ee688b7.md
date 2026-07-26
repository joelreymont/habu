---
title: Require real checkpoint fail-closed
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:29:05.186161+02:00"
---

Why: codex blind review - the landed acceptance evidence for the real-model regression rode a presence-gated leg that silently SKIPs without the checkpoint while the suite reports test: ok; acceptance evidence must not be skippable. Also implements the accepted final-verdict item: the pinned checkpoint becomes mandatory in the DGX Spark release gate. Behavior: a dedicated fail-closed real-artifact entry - hard failure with a distinct code when the pinned checkpoint (sha 248dfc3911869ec493c76e65bf2fcf7f615828b0254c12b473182f0f81d3a707, 548105171 bytes) is absent or wrong - asserting absolute zero ownership counters at isolated entry and exit around the full real-path leg; the DGX gate runs this entry; presence-gated legs remain for artifact-less dev boxes but are never acceptance evidence and their SKIP prints loudly. Owner: the gpt2 suite family plus the gate registration. Dependencies: habu-extract-real-checkpoint-8edea0cd (the provider owns the pinned constants). Acceptance: entry red without the artifact, green with it, counters zero at both stations; gate wiring proven by running the entry through the real gate path; diff lints clean. Real pre-change defect: measured - the suite printed real-model leg SKIPPED and exited 0 on a box without the artifact.
