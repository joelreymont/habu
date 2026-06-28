---
title: Warm gate runner image
status: closed
priority: 1
issue-type: task
created-at: "2026-06-28T19:06:06.597391+02:00"
closed-at: "2026-06-28T20:36:06.179208+02:00"
close-reason: "completed: split phase libs/entries; baked warm runner in early pool; full native gate 80309ms<=90000ms; typed-local-diff-lint, dot-dep-lint, filemap-lint pass"
---

Problem: test/run.f starts each top-level phase as a fresh bin/hb --load common libs plus gate harness files, so the process pool keeps reparsing gate-common/gate-stdlib/gate-engine/diagnostic/build helpers. Existing warm images accelerate inner tools/checker but not the top-level gate runner.

RCA: phase files executed their mains at load time, so they could not be baked directly. A first candidate-built runner was correct functionally but wrong for wall time: it rebuilt the harness image on the critical path and regressed the full gate to 98469ms. The runner is a harness artifact, not the candidate under test; its cache key must cover baked source + seed image. The candidate remains the runtime test subject through HABU_UNDER_TEST.

Fix: split phase files into side-effect-free *-lib.f files plus thin entries, bake the side-effect-free libs into a content-keyed warm runner in the early pool, and have post-runner phases launch hb-gate-warm --load test/gate-runner-entry.f -- PHASE instead of the cold common --load bundle. Files: test/run.f, test/gate-common*.f, test/gate-stdlib*.f, test/gate-engine*.f, test/gate-diagnostics*.f, test/gate-dictionary*.f, test/gate-debug*.f, test/gate-aot-*.f, test/gate-runner-entry.f, test/gate-stats*.f. Acceptance: phase child argv no longer lists the common gate --load bundle after the runner is ready; runner stamp covers baked support files; HABU_UNDER_TEST is passed to candidate-owning checks; focused runner bake/dispatch passes; full native gate passes under 90000ms.
