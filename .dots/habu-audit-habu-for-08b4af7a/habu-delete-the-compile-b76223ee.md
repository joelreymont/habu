---
title: Delete the compile-reset cells nothing reads
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T19:01:28.103515+01:00"
closed-at: "2026-11-13T19:10:07.526452+01:00"
---

Problem: CMBK-CELL, CMFRD-CELL, QPATCH-CELL, QFRAME-CELL and LVD-CELL (src/habu/layout.f, with their src/habu/data-claims.f rows) are written by both kernels' compile reset (src/habu/habu2.f, src/habu/kernel-x64.f) and LVD-CELL also by src/lib/task.f, and nothing reads any of them; they are left from the deleted JIT. Acceptance: the five cells, their claim rows and every store into them are gone; rg -i --no-ignore -w finds none of the names; the arm64 engine builds twice the same and the suite passes; kernel-x64.f is edited by reading only. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-cells
