---
title: Sigmoid BCE and focal loss with VJP
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:19:56.816767+02:00"
---

Binary cross-entropy on logits (numerically stable log-sum-exp form, not sigmoid-then-log) and its focal-loss weighting, for per-class object presence in localization heads. Exact VJPs, gradchecks, and torch-reference fixtures generated the same way as maki/adam-torch-ref-data.f (offline reference values committed as data, not produced by host glue at test time). Builds on FMATH:FEXP and the existing celoss patterns; lives beside maki/celoss.f as its own file, one concern per file.
