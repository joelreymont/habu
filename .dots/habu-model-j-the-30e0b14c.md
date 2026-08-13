---
title: "Model j, the outer loop's index"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-13T05:51:41.958460+02:00\""
---

Found by the do-loop landing (1bfc2749): with plain do modelled, MM-KSTEP-FMA moved from mislabelled E-NELAB-CTRL to E-HIR-UNMODELED naming j - the outer counted loop's index - and the tree has more nested plain-do bodies using it (census usage: j 57 defs). Model it the way i is modelled (the loop frame stack already carries the outer frame; the reader asks the frame one level up). Files: src/compiler/native/{hir-word,elaborate}.f. Depends: none.

Claim: agent=model-j workspace=.jj-ws/habu-model-j
