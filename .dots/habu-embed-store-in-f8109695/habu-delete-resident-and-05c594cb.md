---
title: Delete resident and its erasures
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T22:59:20.807806+02:00"
blocks:
  - habu-cut-gpt2-model-445a19ff
---

Why: after gpt2-model owns WSTORE:store directly, resident, HOLD, and the park/unpark erasures have no product owner. Result: delete WSTORE:resident, HOLD, RESIDENT-DISPOSE, the park/unpark trusted erasures, pre-reserved HOLD cells in every table block, their tests, and their TRUSTED.md rows. Owner: WSTORE resident lifetime deletion only. Production red: dead resident machinery keeps trusted owner erasures and table cells after the hard cut. Acceptance: a boundary-aware sweep finds no deleted word; weight-store and GPT2LOAD suites pass; TRUSTED.md rows are gone; package, trust, typed-local, and exact-diff gates pass. Forbidden: replacement resident, forwarding word, compatibility path, graph-history edit, or unrelated table API change.

Claim: agent=claude workspace=.jj-ws/habu-delete-resident-and-05c594cb
