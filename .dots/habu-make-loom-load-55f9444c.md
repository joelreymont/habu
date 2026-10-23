---
title: Make loom load on current Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T19:20:43.942540+03:00"
---

loom/maki/infer and loom's other Habu sources still require the old lib/... layout (about 396 requires); Habu moved libraries to src/lib/ in 6308d497213. Found by the tier census (habu-census-tier-1-aed47e19): ~/Work/loom/maki/infer/gpt2-serve.f fails 'cannot open .../maki/infer/lib/adt/option.f'. Fix in loom's own repo and workspace as zzusage, blackboard and kiba were (habu-make-zzusage-blackboard-e03d49de); loom is one of the corpora habu-decide-whether-one-358fc93b measures. Claim: unassigned.
