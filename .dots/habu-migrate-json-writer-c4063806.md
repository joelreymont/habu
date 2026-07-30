---
title: Cut tests to explicit JSON
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.649659+02:00"
blocks:
  - habu-add-explicit-json-5d7ee868
---

Why: json-read roundtrip and JSON-WRITE own tests call the singleton directly. Result: each fixture allocates its own aligned writer state, scratch, and destination; threads the linear writer through the final explicit emitters; matches JSON-WRITE:COPY; and closes once. No helper state spans the two suites and no copied serializer appears. Owner and touch points: lib/json-write-test.f and lib/json-read-test.f only. Production red: the provider hard cut cannot pass its direct suites while fixtures retain singleton calls. Acceptance: roundtrip and canonical bytes remain exact; adversarial escaping remains covered; required-capacity paths leave destination sentinels unchanged; both suites contain no JSON-WRITE:$ or zero-argument RESET and remain green beside unmigrated production consumers on the feature branch. Forbidden: production behavior, shared fixture global, adapter, compatibility, version, metric, or lint. Smallest owning check: bin/hb --load lib/json-write-test.f and bin/hb --load lib/json-read-test.f. Claim: unassigned.
