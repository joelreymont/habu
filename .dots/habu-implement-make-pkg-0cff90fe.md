---
title: Implement make-package primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:46.211055+02:00"
---

src/runtime/primitives/package.zig: Create new file. Add make_package(name, nicknames, use_list). Register in global registry. Dependencies: habu-add-global-pkg-2831c53c. Verify: (make-package "FOO").
