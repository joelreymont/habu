---
title: Deflake ir-id activation cleanup case
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T19:21:28.652809+02:00"
---

Full context: test/compiler/ir-id.f case 'activation cleanup permits same-process task reuse' (assert ~52, the E-TASK-STATE activation-failure arm of the concurrency fixture from dot habu-add-compiler-ir-21e976fc) intermittently fails under full-suite parallel load — observed once red (expected 0 got 1, three failures) in a test/run.f execution on the integrated packaging tree, then green on the immediate rerun and 8/8 green standalone on the same engine. Suspected mechanism: under heavy load the fork-based fixture's task activation or its outcome-bounded child hits a timeout/ordering window the fixture treats as failure rather than inadmissible. Root-cause with the fixture's own timeout/ordering evidence (do not just widen a sleep): make the case deterministic under load or grade contended runs inadmissible-and-retry, per the json-perf admissibility convention. Acceptance: the case is proven red under its own mutation (cleanup deleted), and N consecutive full-suite runs under load show no flake.
