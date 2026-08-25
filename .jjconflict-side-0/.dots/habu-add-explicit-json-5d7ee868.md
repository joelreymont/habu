---
title: Add explicit JSON structure emitters
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:04.667322+02:00"
blocks:
  - habu-add-explicit-json-d425d312
---

Why: structural and field helpers should compose the explicit value API without enlarging its owner. Result: package JSON-WRITE adds SEP, OBJ-START, OBJ-END, ARR-START, ARR-END, and PUT-FIELD-S, PUT-FIELD-U, PUT-FIELD-BOOL, PUT-FIELD-NULL, PUT-FIELD-RAW; each consumes and returns JSON-WRITE:writer and calls only the explicit value emitters. These are final names. Owner: JSON structure and field emitters only. Dependency: explicit value emitters. Production red: callers cannot render complete objects or arrays with the new writer. Acceptance: nested object and array fixtures retain byte-exact output, overflow retains exact requirement, two writers interleave, and no helper touches singleton state. Forbidden: caller migration, old-name alias, adapter, raw span, version, compatibility path, allocation, metric, or lint. Smallest owning check: the structure slice of lib/json-write-test.f. Claim: unassigned.
