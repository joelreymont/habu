---
title: Cut build report to explicit JSON
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.479234+02:00"
blocks:
  - habu-add-explicit-json-5d7ee868
---

Why: HB-BUILD:REPORT$, PATH-ERROR$, and JSON quoting use the singleton and return its borrowed buffer. Result: REPORT and PATH-ERROR consume and return a caller-supplied JSON-WRITE:writer through the final explicit emitters; PATH-ERROR-TEXT uses caller writer state for quoting; the hb-build command owns fixed writer state, scratch, and output, matches JSON-WRITE:COPY, and closes once. Delete all raw JSON-span return paths and singleton calls in these consumers while preserving canonical schemas and human text byte-for-byte. Owner and touch points: tools/hb-build-report.f, tools/hb-build-lib.f, tools/hb-build-test.f, and lib/build-cache-test.f only. Production red: report and path-error renders overwrite one ambient buffer. Acceptance: success, cache-path failure, adversarial escaping, exact output, and one-short output use two interleaved writers; refusal changes no destination and returns exact length; hb-build and build-cache suites pass while unrelated singleton consumers remain unchanged on the feature branch. Forbidden: second writer, global state, raw span, adapter, compatibility, version, metric, or lint. Smallest owning check: bin/hb --load tools/hb-build-test.f and bin/hb --load lib/build-cache-test.f. Claim: unassigned.
