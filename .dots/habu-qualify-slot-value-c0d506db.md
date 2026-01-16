---
title: Qualify slot-value metadata lookup
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T11:01:40.901341+02:00"
---

src/runtime/primitives/clos.zig:94-95: slot-value looks up class_metadata by bare class_name from instance vector[0]. Change to qualified lookup.

Files: src/runtime/primitives/clos.zig:94-95
Expected: uses qualified name from symbol
Actual: bare name causes collision
Fix: Extract symbol package, build qualified key for heap.class_metadata.get()
Depends: habu-qualify-class-metadata-0ab680e7
Verify: slot-value on instances from different packages with same class name resolves correctly
