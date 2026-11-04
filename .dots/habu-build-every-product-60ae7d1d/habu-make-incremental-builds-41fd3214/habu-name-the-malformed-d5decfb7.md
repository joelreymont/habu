---
title: Name the malformed-object miss
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T06:41:39.812966+03:00"
closed-at: "2026-10-31T20:52:54.138940+02:00"
close-reason: "Landed in lane 11 4b54ed21: E-NUNIT-PROFILE/E-NUNIT-FILE map to the object-format OBJECT-MISS."
---

OBJECT-READ? (tools/native-definition-cache.f:1409) returns false on a reader refusal (E-NUNIT-PROFILE, E-NUNIT-FILE) without calling OBJECT-MISS, so with HABU_NATIVE_CACHE_DIAG on, a malformed object is a miss with no reason, old fields and R1's new load-row checks alike. Name it as other misses are named. Found by the R1 reader-validation fold (carl-r1).
