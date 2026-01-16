---
title: Update heap findPackage to return Value
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T11:01:20.065754+02:00"
---

src/runtime/heap.zig:~680: findPackage currently returns ?*Package. Change to return ?Value to match primitive API.

Files: src/runtime/heap.zig findPackage signature, all call sites
Expected: findPackage returns Value wrapping Package
Actual: returns raw pointer
Fix: Change return type to ?Value, wrap Package* with Value.fromPackage before return
Depends: habu-add-pkg-value-e1256342
Verify: build succeeds, call sites updated
