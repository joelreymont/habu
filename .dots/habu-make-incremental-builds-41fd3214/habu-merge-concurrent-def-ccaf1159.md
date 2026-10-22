---
title: Merge concurrent definition container writes
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T12:18:15.966481+03:00"
---

Problem: a container is read whole at open (tools/native-definition-cache.f:591-606) and rewritten whole at close (:639-643, ATOMIC-WRITE-FILE) with no lock or merge: two builds sharing a root that load the same file each publish and the later rename discards the earlier build's definitions (lost reuse, not wrong code). Acceptance: merge under the WORK-OPEN flock (or per-definition entries); E2E: two concurrent builds on one cache, both products equal the serial product and a third build reuses both builds' definitions. Files: tools/native-definition-cache.f. Depends: Keep definition containers in the selected cache root. Claim: unassigned.
