---
title: "Find a changed site's owner without scanning every ndict record"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T00:24:17.533216+02:00"
---

Problem: tools/native-unit-object.f EARLIER-WRITE-LINE (:1355) calls OWNER-AT (:1327) once per changed site, and OWNER-AT loops over every ndict record calling REC-DATA? (:1320), NFILE-REF:DATA-ORIGIN-BASE and DATA-ORIGIN-RECORD (tools/native-file-reference.f:1203, :1191): sites x records per load action, reached from EARLIER-REFUSE (:3049) and LOAD-ACTION (:1444). With HABU_NATIVE_CACHE_DIAG set the scan is 66.4% of a cold engine build's samples, 59.8% warm and 31.6% on the base tree (cost lane, carl-fasl2/.out). Acceptance: a changed site's owner is found without a per-site pass over all records; the diag output is unchanged; OWNER-AT and its callees are under 5% of a diag-on cold build's samples. Files: tools/native-unit-object.f, tools/native-file-reference.f. Verify: HABU_NATIVE_CACHE_DIAG=1 hb-build --engine sampled with sample(1) before and after; test/aot-payload-graph and the native cache tests green. Depends: none. Ownership: the two files. Claim: unassigned.
