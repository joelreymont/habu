---
title: "CAD: FFN flagship demo"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:50.084787+02:00"
---

docs/model-cad.md flagship. FFN: linear->bias->GELU->linear->residual->norm. Committed demo test: model defined in REPL, fusion plan shown, traffic before/after, coalescing report, schedule candidates, golden pass, gradcheck pass if backward, profile row, artifact promoted, comparison vs unfused baseline. Win = fusion/traffic reduction; no tensor-core parity claim. Related: habu-small-model-end. Depends: cad-7.
