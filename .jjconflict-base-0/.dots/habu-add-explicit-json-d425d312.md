---
title: Add explicit JSON value emitters
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:04.523226+02:00"
blocks:
  - habu-build-explicit-json-399f5929
---

Why: the caller-owned writer needs a final value API that can coexist with the untouched singleton only inside the unpublished cutover branch. Result: package JSON-WRITE adds PUT-RAW, PUT-U, PUT-S, PUT-BOOL, PUT-NULL, and PUT-KEY; each consumes and returns JSON-WRITE:writer, uses the writer core, preserves exact current escaping and decimal bytes, and records exact required length after scratch exhaustion. The names are the final API and no old word forwards to them. Owner: value and key emitters only. Dependency: explicit writer state core. Production red: the new writer cannot emit a JSON value. Acceptance: every byte class, signed boundary used by callers, exact scratch, one-short scratch, two interleaved writers, and checked drop or duplicate negatives execute through the real emitters. Forbidden: structure or field helper, global state, old-name alias, adapter, version, compatibility path, allocation, metric, or lint. Smallest owning check: the value slice of lib/json-write-test.f. Claim: unassigned.
