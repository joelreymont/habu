---
title: Re-run Habu softmax pass@k with corrected ROW-STORE spec
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T15:13:15.788978+02:00"
blocks:
  - habu-commit-checked-habu-a8ab5f56
---

The model-driven softmax pass@1 (3/5) in docs/eval-triton.md is CONFOUNDED: the generator prompt mis-specified ROW-STORE arg order as (span ctx tile); real order is (tile span ctx) (tile deepest; canonical idiom leaves the tile on the stack then appends span+ctx). 2/5 samples followed the wrong spec -> author-reject, so 3/5 reflects the prompt error, not the DSL. FIX: re-run the 5 independent softmax generators with the CORRECTED op spec (ROW-STORE ( tile span ctx -- )), grade via the committed grader (dot habu-commit-checked-habu-a8ab5f56), record the unbiased pass@1 in docs/eval-triton.md, keep the original caveat as the methodology lesson. VERIFY: docs updated with corrected pass@1. Deps: after the committed Habu grader.
