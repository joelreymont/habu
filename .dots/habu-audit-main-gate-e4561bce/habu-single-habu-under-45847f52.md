---
title: Single Habu-under-test artifact
status: closed
priority: 1
issue-type: task
created-at: "2026-06-28T19:06:06.587204+02:00"
closed-at: "2026-06-28T19:57:39.574731+02:00"
close-reason: "completed: engine build publishes a shared HABU_UNDER_TEST artifact, prints path+sha256, downstream phase launches switch to it after publication, baseline tests keep bin/hb; proof full native gate PASS 87367ms <= 90000ms with under-phase=15 under-env=16 candidate=1"
---

Problem: test/gate-engine.f builds hb-new in the engine-build slice temp root, but all other main-gate phases still execute bin/hb. This means the gate proves a rebuilt candidate only in limited engine checks while stdlib/check/diagnostic/tool phases exercise the seed/current bin/hb. Fix: add a first-class HABU_UNDER_TEST artifact built once under the main gate root, keep it for downstream phases, and pass it through PROC env/argv helpers. Preserve explicit baseline-contract tests against bin/hb only where seed/current binary behavior is the invariant. Files: test/run.f, test/gate-common.f, test/gate-engine.f, tools/build-fixpoint.f. Acceptance: build candidate once; downstream phase launch paths use HABU_UNDER_TEST; engine suite still compares candidate against baseline where needed; full native gate passes; failure output reports the artifact path and source hash.
