---
title: Share one keyed saved-builder image across the native builder rows
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T13:22:56.565453+02:00"
---

Problem (review 313 of e71a4aae, nbimage): test/native-builder-image-e2e.f, -whitebox.f and -refusals.f each save their own builder (APP-IMAGE:SAVE of tools/native-builder-image.f, ~13-19 s CPU, 36-110 s wall at load) on every gate, never cached, on each row's critical path. And the dropped sealed-product parity (the saved builder's sealed engine byte-equal, with .names, to tools/native-build.f's from the same tree) is checked by no gate row: the seal pass (src/core/internal-mark.f IMK-PASS/IMK-MARK) marks from checker rows the saved builder restored, which whitebox parity cannot see. Acceptance: a 'saved-builder' keyed family in test/gate-images.f built on test/keyed-image.f (shape of test/app-image-engine.f; closure tools/native-builder-image.f), the three rows use its PATH$; measured CPU per gate before/after; then decide sealed parity on measurement: if a sealed build row fits the registry's 180 s-in-pool rule (test/gate-stdlib-cases.f:7-18), add it with .names comparison; otherwise record the measured reason in the lib's claim list. Files: test/gate-images.f, test/native-builder-image-*.f, docs/gate.md (keyed image count).
