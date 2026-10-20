---
title: Type child parser and exit fixtures
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:17:50.871891+03:00"
closed-at: "2026-10-19T13:36:54.888841+03:00"
close-reason: Child parser catch subjects use the exact typed parser through a preserving adapter and discard restored operands after refusal. Exit fixtures pass an actual empty string. Process and complete scope-refusal cases pass with original outcomes; independent Astra review clear.
---

Matching native C2 scope-refusal subjects fail at catch before intended parser diagnostics in five cases with numeric4 and TYPED-BUFFER or LAYOUT-BUFFER. The broader process test also cannot reach its three-byte stderr expectation because child 0 0 7 die correctly fails typed pointer checking. Diagnose exact required argument/call contracts and adapt Habu fixtures at explicit boundaries, preserving parser recovery markers, actual process exit/stderr behavior and all expected outcomes. Do not weaken core top checks or broaden checker authority.
