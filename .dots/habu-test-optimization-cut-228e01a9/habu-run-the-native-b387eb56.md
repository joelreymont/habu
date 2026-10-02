---
title: Run the native-builder image e2e test
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T11:07:24.724715+02:00"
---

Problem (r4-mastermerge worker 4): test/native-builder-image-e2e.f asserts 4, 6, 8 (:263, :271, :279) expect the usage text from before --target existed, while tools/native-build-args.f:73,100 prints the newer text, so the test fails (rc 1) on the line, on master and at the merge; and nothing in test/, tools/ or docs/ runs it, so the gate never saw it. Acceptance: decide whether the test claims something no gate row covers (it builds images through the native builder); if it does, the assertions pin what the usage must say (the option names, not the whole text) and the file is a registered gate row (docs/gate.md) that passes; if every claim is covered elsewhere, delete it and name the covering rows. Files: test/native-builder-image-e2e.f, the gate table. Base: the merged top.
