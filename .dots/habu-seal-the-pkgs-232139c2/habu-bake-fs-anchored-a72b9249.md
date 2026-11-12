---
title: Bake fs-anchored.f; four FS tests stop reopening FS
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:44:38.994731+02:00"
---

Problem: src/lib/fs-anchored.f:12 opens baked package FS from a loaded file, and fs-anchored-test.f:8, fs-cause-test.f:12, fs-rename-noreplace-test.f:11 and test/source-root-exe-test.f:20 reopen FS to forward its privates; under the boundary seal each exits 84. Acceptance: hb bakes fs-anchored.f (tools/hb-build-core.f RUNTIME-FILES) and the four tests open no FS and pass on the rebuilt hb. Design: .jj-ws.noindex/jerry-seal/.out/seal-design.md. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/seal-fs-a.
