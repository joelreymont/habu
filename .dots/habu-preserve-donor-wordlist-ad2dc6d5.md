---
title: Preserve donor wordlist visibility
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T09:01:00.376670+03:00"
---

AOT donor source lookup conflates public and private rows in the same package, so an exported alias can inherit the private definition source. Match exact span/name and namespace visibility from target and donor metadata, and check downstream stripped AOT map on native-map-b/c fixture.
