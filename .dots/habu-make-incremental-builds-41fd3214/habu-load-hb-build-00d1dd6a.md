---
title: Load hb-build without recompiling it
status: open
priority: 3
issue-type: task
created-at: "2026-10-22T14:59:32.133121+03:00"
---

Problem: every hb-build invocation compiles tools/hb-build.f from source: 0.37 s, half of a 0.74 s no-change build and a fixed cost on every test that calls hb-build. Evidence: docs/compiler-measurements.md section 11. Acceptance: hb-build runs from a cached captured image keyed on its sources and the engine, rebuilt only when they change; no-change build wall measured before/after; outputs unchanged. Files: tools/hb-build.f, tools/hb-build-lib.f, bin/hb launcher path. Verify: no-change and warm engine build timings, test/run.f rows that call hb-build. Depends: none. Needs a Plan design first. Claim: unassigned.
