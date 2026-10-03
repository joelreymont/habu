---
title: "Size check.f's dependency closure for the native builder"
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T12:35:54.039649+02:00"
---

Problem (lane 295 dupdiag): bin/hb --load tools/check.f -- tools/native-build.f (and -- tools/native-build-core.f) exits 67 'hb: uncaught throw code -3000' (E-TBL-BOUNDS) with no diagnostic, before and after a4da213d; likely the closure tables bounded by CHK-DEP-MAX 128 (tools/check-core.f:64). Acceptance: find the bound it overruns; check.f either holds the native builder's whole closure (sized to what it accepts, as fbb5f723 did for reports) or refuses with a named, located diagnostic and its documented exit code, never an uncaught throw; tools/check.f -- tools/native-build.f gives its verdict, seen failing first. Files: tools/check-core.f, tools/check-test-lib.f.
