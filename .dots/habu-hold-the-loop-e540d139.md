---
title: "Hold the loop stack's address, not its depth"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:30:09.406596+01:00"
---

Problem: LOOPSP-CELL (src/habu/layout.f) counts DO/LOOP frames, so every loop word shifts it and adds LOOP-BASE-CELL: 18 bytes on x86-64, three instructions on arm64; tenderd has 5,205 such sequences and 13,813 disp32 accesses of the cell. Acceptance: the cell holds the address past the innermost frame and lies below $80 in DATA; code shrinks and a counted loop is faster on both hosts. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-low.
