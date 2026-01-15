---
title: Add *package* special variable
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:22.267782+02:00"
---

src/interp/vm.zig: Add *package* global variable initialized to CL-USER. Track current package for reader. Dependencies: habu-implement-rename-pkg-ce362bcb. Verify: *package* accessible.
