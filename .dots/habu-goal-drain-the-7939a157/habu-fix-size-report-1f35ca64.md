---
title: Fix size-report review findings
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-18T23:07:03.166881+02:00\""
closed-at: "2026-07-18T23:36:45.059469+02:00"
---

Review findings on the Mac agent's size-attribution work (all in tools/, no maki collision). (1) tools/size-report.f PAGE (lines 182-185) silently falls back to the macOS page size when the build target is neither macOS nor Linux - a silent fallback; make the unknown-target case die with a named message instead. (2) SUM-ALL / SUM-CONTAINER / SUM-TEXT / CODE-TOTAL are four copies of the same row-summing loop differing only in the row predicate; fold them into one predicate-parameterized sum using a checked quotation argument (the [: ... ;] pattern tools/ddc-scheduled.f already uses with WALK-FILES), keeping the four public names as one-line wrappers. (3) tools/ddc-scheduled-test.f and tools/ddc-verify-test.f never run in any gate - both are cheap (fake audit seam / comparison core only, no real bootstrap), so route them through the forked include list in test/gate-stdlib-inline-lib.f like tools/size-report-test.f. Full gate must stay green.
