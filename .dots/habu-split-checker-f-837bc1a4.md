---
title: Split checker.f along Foundation A seams
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:05:59.658027+02:00"
blocks:
  - habu-foundation-a1-declarable-98aebe7b
---

size-review item 7. 9,085 lines, zero section banners, 5.7% of all Forth in one file. Split (role algebra / effect rows / diagnostics / TFAM glue) AS PART OF Foundation A landing (habu-foundation-a1-declarable-98aebe7b), not before and not separately — one churn window, fixpoint+gate proven.

Edge note 2026-07-18 (maki orchestrator): blocker repointed
habu-foundation-a-declarable-0390600f -> habu-foundation-a1-declarable-98aebe7b
(the A1-campaign dedupe renamed the dot but this edge was missed; dot-dep-lint
went red on master at ea489ec0 and is repaired here).
