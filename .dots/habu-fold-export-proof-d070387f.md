---
title: Fold EXPORT proof into cache race
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T04:45:50.678921+03:00"
---

hb-build-aot-cache currently spends130.873 sampledCPU: four successful application builds independently cover EXPORT same-body equality(two) and source-A-to-B cache race(two). Use the race B product as the alias variant and add only its non-alias reference build on the same source/output paths: three builds preserve exact executable equality, both alias calls,111-to222 source race, corrupted-map object recovery and final artifact restore. Keep W as nontrivial dup* so calls remain real. Existing real B object recovery also supersedes synthetic positive object-hit fixture; preserve invalid-object/ABI/publication cases. Measure whole row CPU/wall before/after, retain all distinct boundary assertions, independent review.
