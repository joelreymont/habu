---
title: Remove circular package helper dependencies
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:24:44.037256+02:00"
---

Files: stdlib.habu around line 4143-4165
%package-symbols-list and %package-exports-list may have circular deps.
Already converted from maphash to dolist, verify no other issues.
Clean up any remaining circular references.
Verify: Functions compile successfully in isolation.
Est: 15min
