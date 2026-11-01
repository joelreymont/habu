---
title: "Give snapshot-writer's grid framing case a pad bit by construction"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T19:07:10.618360+02:00"
---

Problem: test/snapshot-writer.f GRID-FRAMING-CASE (~635-650) asserts GRID-G CELL-GRID:CELL-BITS mod 0<> (a pad bit after the last group) about whatever snapshot the test takes, so its premise depends on the image's incidental size. On C4a's x86 engine (T0 at tier 0, jerry-t0x86 uqstoymz) G lands on a multiple: F91 (the premise) fails, F93 doctors a real presence bit and the image loads (expected 79, got 0), and F94 follows; arm64 passes. Acceptance: the case makes a snapshot whose group count leaves a pad bit by construction (or asserts the refusal on a map built for it), on every target; no premise about the running image's size. Files: test/snapshot-writer.f. Verify: snapshot-writer on arm64 and on the x86 peer, F91/F93/F94 gone. Depends: none. Ownership: test/snapshot-writer.f grid cases. Claim: unassigned.
