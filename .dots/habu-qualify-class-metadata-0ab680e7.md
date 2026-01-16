---
title: Qualify class metadata keys by package
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T11:01:28.910737+02:00"
---

src/compiler/compile.zig:5591,5601: class_metadata uses bare class_name as key. Change to qualified name "PKG::CLASS".

Files: src/compiler/compile.zig compileDefclass
Repro: (in-package "FOO") (defclass bar ()) (in-package "BAZ") (defclass bar ()) - second overwrites first
Expected: metadata keys "FOO::bar" and "BAZ::bar"
Actual: both use "bar", collision
Fix: At line 5600, use self.qualifyName(class_name) instead of bare class_name for metadata key
Verify: two classes same name different packages don't collide
