---
title: Implement package-nicknames primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:59.463177+02:00"
---

src/runtime/primitives/package.zig: Add package_nicknames(pkg). Return list of nickname strings. Dependencies: habu-implement-pkg-name-3e327571. Verify: (package-nicknames *package*).
