---
title: "Judge a skipped arm's loader form in the walk as the load does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:46:57.258718+01:00"
---

Problem: ': require ( -- ) ;' inside 'HB-TARGET-WASM? [if] ... [then]' then 'require v-ok.f' live: plain tools/check.f rejects E-LOADER-FORM at 3:1 (the walk follows every arm), while --all-errors drops it because the composition, honouring the target, never rejects it. Acceptance: the walk and the composition agree whether a definition in a skipped arm shadows a loader word, and both modes print the same records. Depends: none. Claim: unassigned.
