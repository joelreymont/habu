---
title: Retire the straight-line allocator path
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-03T16:22:48.097365+02:00\""
---

regalloc.f's own header states the retirement condition - 'the day the general path can anchor a spill decision to a block is the day the first half is retired' - and block-anchored spill plans landed. Unify: one allocation path (the multi-block machinery, which subsumes single-block as N=1), deleting the straight-line scan, its separate spill logic, and the CALLS-MB? dispatch seam (subsumes habu-unify-the-two-d4f93e83 - close it with this). Every existing exact-register fixture must pass unchanged OR its expectation move deliberately with the reason (the two paths' victim rules differ - furthest-next-use both, but tie-breaks and slot assignment order may produce different exact registers; a wholesale expectation migration is acceptable if argued, silent drift is not). Net lines strongly negative. All four corpora byte-identical old columns, results identical, cost within the control band.

Claim: agent=retirelane workspace=.jj-ws/habu-retire-the-straight-a774fe0e
