---
title: Warm gate runner image
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T19:06:06.597391+02:00"
---

Problem: test/run.f starts each top-level phase as a fresh bin/hb --load common libs plus gate harness files, so the process pool keeps reparsing gate-common/gate-stdlib/gate-engine/diagnostic/build helpers. Existing warm images accelerate inner tools/checker but not the top-level gate runner. Fix: bake a content-keyed gate-runner snapshot after HABU_UNDER_TEST is built, with common gate libs and phase dispatch loaded; top-level phase children launch the warm runner with a phase id instead of cold --load. Files: test/run.f, tools/warm-image-lib.f, test/gate-common.f, test/gate-stdlib.f, test/gate-engine.f, test/gate-diagnostics.f. Acceptance: phase child argv no longer lists the common gate --load bundle; warm runner stamp includes seed/candidate/source inputs; focused phase timings improve; full native gate passes.
