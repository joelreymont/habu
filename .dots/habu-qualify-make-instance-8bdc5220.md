---
title: Qualify make-instance metadata lookup
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T11:01:34.948214+02:00"
---

src/compiler/compile.zig:5699: compileMakeInstance looks up metadata by bare class_name. Change to qualified lookup matching symbol package.

Files: src/compiler/compile.zig:5699-5710
Expected: metadata lookup uses "PKG::CLASS" from symbol's package
Actual: uses bare name from current package
Fix: If class_val is symbol, extract its package, build qualified key for metadata.get()
Depends: habu-qualify-class-metadata-0ab680e7
Verify: (in-package "FOO") (make-instance 'bar) uses FOO::bar metadata
