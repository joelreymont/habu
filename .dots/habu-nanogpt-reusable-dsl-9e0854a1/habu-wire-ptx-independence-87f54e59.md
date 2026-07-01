---
title: Wire PTX independence lint
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-01T22:29:57.605111+02:00\\\"\""
closed-at: "2026-07-01T22:43:37.184627+02:00"
close-reason: "completed: maki-dep-lint now scans generic tools/ptx, softmax-gradcheck no longer requires maki/array.f, focused maki-dep fixtures/live lint, typed-local diff lint, and lint-tools passed"
---

File: PLAN.md:29; cause: generic tools/ptx paths can still require maki files, so layering is review-only; fix: extend maki-dep-lint or a PTX-owned lint over lib/ptx and generic tools/ptx, migrate Maki-dependent helpers into neutral PTX support plus Maki adapters, and wire the negative fixture into lint-libs-ptx-tool or the focused PTX gate; deps: none; verification: a tools/ptx fixture containing a maki token fails the lint slice while Maki adapters still pass their own gate.
