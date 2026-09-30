---
title: Walk the gate-images graph in the entry guard
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:49.944297+02:00"
closed-at: "2026-09-30T15:13:06.450166+02:00"
close-reason: Landed d8867baf. Guard 29 s -> ~0.03 s (+1.6 s DERIVE shared with START); grants identical for all 499 rows; field-proj double run fixed; 11 mutations red. Fable review accepted. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit).
---

Problem: the gate's entry guard lexed 1,052 files 17,688 times per gate, serially in SUITE-SETUP: 29.0 s at load 6.6, 76.4 s at load 71. Acceptance: the guard walks the load graph test/gate-images.f already derives, reading each file once; it still refuses an import or launch of another row's entry; the derived image grants are unchanged for all rows; field-proj no longer runs twice. Verify: guard time; grants compared for every row; mutations of the guard's refusals; full native gate.
